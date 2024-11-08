package fr.qln.sortspotifyscala.server

import cats.data.Kleisli
import cats.effect.{Async, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fr.qln.sortspotifyscala.models.config.{ClientConfig, ServerConfig}
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.implicits.*
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.{ApplicativeServiceError, HandleDomainError}
import fr.qln.sortspotifyscala.services.*
import fr.qln.sortspotifyscala.services.auth.*
import fr.qln.sortspotifyscala.services.spotify.{AlbumAlg, ArtistAlg, GenresAlg, PlaylistAlg, UserAlg}
import fr.qln.sortspotifyscala.services.utils.*
import fs2.io.net.Network
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.middleware.Logger as Http4sLogger
import org.http4s.{HttpApp, HttpRoutes}
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.concurrent.duration.{Duration, SECONDS}


object SortSpotifyServer {

  
  def run[F[_]:Async :Network: HandleDomainError: ApplicativeServiceError]: F[Nothing] = {

    implicit val logging: LoggerFactory[F] = Slf4jFactory.create[F]
    val configService = new ConfigServiceImpl[F](ParseFilterService.create[F]())
    for {
       
      serverConfig: ServerConfig <- Resource.eval(configService.getServerConfig)

      sessionStoreService: SessionStore[F] = new SessionStoreImpl[F]
      sessionService: SessionService[F] = new SessionServiceImpl[F](sessionStoreService)

      clientService: ClientService[F] = ClientService.create(sessionService, ClientConfig(Duration(5, SECONDS), 5))

      authStoreService: AuthStoreService[F] = new AuthStoreServiceImpl[F]
      authService: AuthServiceImpl[F] = new AuthServiceImpl[F](clientService = clientService, authStore = authStoreService, sessionService = sessionService, serverConfig = serverConfig)
      
      debugMiddleware: DebugMiddleware[F] = new DebugMiddlewareImpl 
      
      albumAlg = AlbumAlg.create[F](clientService)
      artistAlg = ArtistAlg.create[F](clientService)
      userAlg = UserAlg.create[F](clientService)
      playlistAlg = PlaylistAlg.create[F](sessionService, clientService)
      genresAlg = GenresAlg.create[F](albumAlg, artistAlg)
      
      sortService = SortService.create[F](userAlg)
      
      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract segments not checked
      // in the underlying routes.
      loginRoutes: HttpRoutes[F] =  SortSpotifyRoutes.loginRoutes[F](authService, sessionService)
      
      authCallBackRoutes: HttpRoutes[F] = sessionService.sessionRequired(
          SortSpotifyRoutes.authCallbackRoutes[F](config = serverConfig, authService = authService, sessionService = sessionService, userProfileAlg = userAlg)
        )

      sortRoutes: HttpRoutes[F] =  sessionService.sessionRequired(
        SortSpotifyRoutes.sortRoutes(sessionService, genresAlg, configService, sortService, userAlg, playlistAlg)
      )
      
      finalRoutes = (
          loginRoutes <+> 
          authCallBackRoutes <+> 
          SortSpotifyRoutes.homeRoutes() <+> 
          sortRoutes
        ).orNotFound
      
      // With Middlewares in place
      finalHttpApp: HttpApp[F] = Http4sLogger.httpApp(true, true)(finalRoutes)

      _ <-
        EmberServerBuilder.default[F]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(finalHttpApp)
          .build
    } yield ()
  }.useForever


}
