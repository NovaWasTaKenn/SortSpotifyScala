package fr.qln.sortspotifyscala.services.auth

import cats.data.{Kleisli, OptionT}
import cats.effect.Async
import cats.mtl.implicits.*
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fr.qln.sortspotifyscala.models.errors.DomainErrors.{SessionError, SessionExpired, SessionNotFoundInRequest, SessionNotFoundInStore}
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.{ApplicativeSessionServiceError, HandleSessionError}
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, RequestCookie, Response}
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}

import java.time.LocalDateTime
import java.util.UUID

type SessionId = UUID

case class Session(sessionId: SessionId, authed: Boolean, expirationTime: LocalDateTime, tokens: Option[Tokens], spotifyUserId: Option[String])

trait SessionService[F[_]] {

  def newSession(authed: Boolean, tokens: Option[Tokens], spotifyUserId: Option[String]): F[Session]
  def modifySession(newSession: Session): F[Boolean]
  def getSessionId(cookies: Iterable[RequestCookie]): F[SessionId]
  def removeSession(sessionId: SessionId): F[Boolean]
  def getStoredSession(sessionId: SessionId): F[Session]
  def sessionRequired(service: HttpRoutes[F]): HttpRoutes[F]
}

class SessionServiceImpl[F[_]: Async : LoggerFactory : HandleSessionError : ApplicativeSessionServiceError]
(sessionStore: SessionStore[F]) extends SessionService[F] {


  private val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

  private def removeSession(sessionId: SessionId, sessionStore: SessionStore[F]): F[Boolean] =
    sessionStore.removeSession(sessionId)

  def removeSession(sessionId: SessionId): F[Boolean] =
    removeSession(sessionId, sessionStore)


  //sessionId should be 64 bits of entropy 16 hexadecimal chars with meaningless data and imprecise name
  private def getNewSessionId(sessionStore: SessionStore[F], retriesLeft: Int = 5): F[SessionId] =
    val newSessionId: SessionId = UUID.randomUUID()

    for {
      alreadyExists <- sessionStore.exists(newSessionId)
      sessionId <- !alreadyExists match
        case true => newSessionId.pure[F]
        case false if retriesLeft <= 0 => new Throwable("Couldn't find unused UUID for session id").raiseError[F, SessionId]
        case false => getNewSessionId(sessionStore, retriesLeft - 1)

    } yield sessionId


  private def newSession(authed: Boolean, sessionDurationSeconds: Long, tokens: Option[Tokens], spotifyUserName: Option[String], sessionStore: SessionStore[F]): F[Session] =

    for {
      _ <- logger.info(s"Creating new session")
      newSessionId <- getNewSessionId(sessionStore)
      expirationTime = LocalDateTime.now.plusSeconds(sessionDurationSeconds)
      newSession <- Session(sessionId = newSessionId, authed= authed, expirationTime = expirationTime, tokens = tokens, spotifyUserId= spotifyUserName).pure[F]
      _ <- sessionStore.addSession(newSession)
      storedNewSession <- getStoredSession(newSessionId)
      _ <-   logger.debug(s"Stored session: $storedNewSession")
    } yield storedNewSession


  def newSession(authed: Boolean, tokens: Option[Tokens], spotifyUserId: Option[String]): F[Session] =
    newSession(authed, 7200L, tokens, spotifyUserId, sessionStore)




  private def modifySession(session: Session, sessionStore: SessionStore[F]): F[Boolean] =
    sessionStore.modifySession(session.sessionId, session)

  def modifySession(newSession: Session): F[Boolean] = modifySession(newSession, sessionStore)

  def getStoredSession(sessionId: SessionId): F[Session] =
    sessionStore.getSession(sessionId)

  def getSessionId(cookies: Iterable[RequestCookie]): F[SessionId] =
    for {
      _ <- logger.info("Getting session cookie")
      sessionCookie <- cookies.find(_.name == "sessionId").map(cookie => UUID.fromString(cookie.content)) match {
        case Some(sessionCookie) => sessionCookie.pure[F]
        case None => SessionNotFoundInRequest.raise[F, SessionId]
      }
    } yield sessionCookie


  def sessionRequired(service: HttpRoutes[F]): HttpRoutes[F] = Kleisli { req =>

    val dsl = new Http4sDsl[F] {}
    import dsl.*

    val response: F[Option[Response[F]]] =
      for {
        _ <- logger.info(s"Session required. Checking if session is valid")
        _ <- logger.debug(s"Request cookies: ${req.cookies}")
        receivedSessionId <- getSessionId(req.cookies)
        _ <-logger.info("Session found in request")
          
        storedSession <- sessionStore.getSession(receivedSessionId)
        _ <- logger.info("Session found in session store")
        _ <- storedSession.expirationTime.isAfter(LocalDateTime.now) match {
          case true => logger.info("Session not expired") >> true.pure[F]
          case false => SessionExpired.raise[F, Boolean]
        }
        _ <- logger.info("Session validated")
        response <- service(req).value
        _ <- logger.debug(s"Session middleware Response: $response")

      } yield response


    val errorHandledResponse = response.handleWith[SessionError]({
        case error@SessionNotFoundInStore =>
          logger.error(error)("Session not found in session store") >> BadRequest("Session not found in session store").map(resp => Some(resp))
        case error@SessionNotFoundInRequest =>
          logger.error(error)("Session not found in session request") >> BadRequest("Session not found in session request").map(resp => Some(resp))
        case error@SessionExpired =>
          logger.error(error)("Session expired") >> BadRequest("Session expired").map(resp => Some(resp))
        case error: SessionError =>
          logger.error(error)(s"Unexpected domain error: ${error.getMessage}") >> BadRequest(s"Unexpected domain error: ${error.getMessage}").map(resp => Some(resp))
      })
      .recoverWith({
        case error: Throwable => logger.error(error)("Internal server error") >> InternalServerError(error.getMessage).map(resp => Some(resp))
      })

    OptionT(errorHandledResponse)

  }
}



