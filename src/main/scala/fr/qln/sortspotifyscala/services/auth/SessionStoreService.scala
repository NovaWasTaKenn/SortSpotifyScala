package fr.qln.sortspotifyscala.services.auth

import cats.effect.Async
import cats.mtl.Raise
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fr.qln.sortspotifyscala.models.errors.DomainErrors.{SessionAlreadyExists, SessionError, SessionNotFoundInStore}
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.{ApplicativeSessionServiceError, RaiseSessionError}
import org.typelevel.log4cats.LoggerFactory

import scala.collection.concurrent.TrieMap

trait SessionStore[F[_]]:

   val getSession: SessionId => F[Session]
   val removeSession: SessionId => F[Boolean]
   val modifySession: (SessionId, Session) => F[Boolean]
   val addSession: Session => F[Boolean]
   val exists: SessionId => F[Boolean]

class SessionStoreImpl[F[_]: Async: LoggerFactory: RaiseSessionError: ApplicativeSessionServiceError] extends SessionStore[F]:

  val sessionStore: TrieMap[SessionId, Session] = TrieMap.empty
  
  val getSession: SessionId => F[Session] = getSession(_, sessionStore)
  val removeSession: SessionId => F[Boolean] = removeSession(_, sessionStore)
  
  val modifySession: (SessionId, Session) => F[Boolean] = modifySession(_, _, sessionStore)
  val addSession: Session => F[Boolean] = addSession(_, sessionStore)
  val exists: SessionId => F[Boolean] = exists(_, sessionStore)


  private def getSession(sessionId: SessionId, sessionStore: TrieMap[SessionId, Session]): F[Session] =
    optionToSessionNotFound(sessionStore.get(sessionId: SessionId))

  private def removeSession(sessionId: SessionId, sessionStore: TrieMap[SessionId, Session]): F[Boolean] =
      optionToSessionNotFound(sessionStore.remove(sessionId:SessionId)).map(_=>true)

  private def modifySession(sessionId: SessionId, newSession: Session, sessionStore: TrieMap[SessionId, Session]): F[Boolean] =
      optionToSessionNotFound( sessionStore.put(sessionId, newSession)).map(_=>true)

  private def addSession(session: Session, sessionStore: TrieMap[SessionId, Session]): F[Boolean] =
    for {
      sessionOption <- sessionStore.put(session.sessionId, session).pure[F]
      sessionSuccessfullyAdded <- sessionOption match {
        case Some(_) => Raise[F, SessionError].raise(SessionAlreadyExists)
        case None => true.pure[F]
      }
    } yield sessionSuccessfullyAdded

  private def exists(sessionId: SessionId, sessionStore: TrieMap[SessionId, Session]): F[Boolean] = sessionStore.exists(_._1==sessionId).pure[F]


  private def optionToSessionNotFound(option: Option[Session]): F[Session] =

    for {
      sessionOption: Option[Session] <- option.pure[F]
      session <- sessionOption match {
        case Some(session) => session.pure[F]
        case None => Raise[F, SessionError].raise(SessionNotFoundInStore)
      }
    } yield session