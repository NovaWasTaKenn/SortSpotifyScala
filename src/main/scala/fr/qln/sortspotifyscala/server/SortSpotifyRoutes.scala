package fr.qln.sortspotifyscala
package server

import cats.effect.{Concurrent, Temporal}
import cats.mtl.implicits.*
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.DomainErrors.AuthenticationError
import fr.qln.sortspotifyscala.models.Types.{ApplicativeAuthServiceError, HandleAuthError}
import fr.qln.sortspotifyscala.services.AuthService
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.{HttpRoutes, Request}
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}


object SortSpotifyRoutes {

  private object StateQueryParamMatcher extends QueryParamDecoderMatcher[String]("state")
  private object CodeQueryParamMatcher extends QueryParamDecoderMatcher[String]("code")

  private object ErrorQueryParamMatcher extends QueryParamDecoderMatcher[String]("error")

  val clientId: String = "09f86191c869489b84914d1172051c61"
  val scopes: String = "user-read-private user-read-email"
  val callbackUrl: String = "http://localhost:8080/callback"
  val authUrl: String = "https://accounts.spotify.com/authorize"
  val tokenUrl: String = "https://accounts.spotify.com/api/token"
  val homeUrl: String = "http://localhost:8080/home"



  
  

  def authRoutes[F[_]: LoggerFactory : Concurrent: Temporal: HandleAuthError: ApplicativeAuthServiceError](authService: AuthService[F]): HttpRoutes[F] = {

    val dsl = new Http4sDsl[F] {}
    import dsl.*

    val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

    HttpRoutes.of[F] {

      case GET -> Root / "login" =>
        
        val authorizeUrl = authService.redirectUrl
        
        for {
          resp <- PermanentRedirect(authorizeUrl)
        } yield resp


      case GET -> Root / "callback" :? CodeQueryParamMatcher(code) +& StateQueryParamMatcher(returnedState) =>

        authService.getInitialTokens(code = code, returnedState =  returnedState)
          .flatMap(_=>PermanentRedirect(homeUrl))
          .handleWith[AuthenticationError]({
            case error: AuthenticationError =>
              logger.error(error)("Error in authentication") >> BadRequest("Authentication error") })
          .recoverWith({
            case error: Throwable => logger.error(error)("Internal server error") >> InternalServerError("Internal server error") })


      case GET -> Root / "callback" :? ErrorQueryParamMatcher(error)   +& StateQueryParamMatcher(state) => BadRequest(error)
        
    }
  }

  def helloWorldRoutes[F[_] : Concurrent](): HttpRoutes[F] =
    ???

}
