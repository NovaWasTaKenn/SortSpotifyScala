package fr.qln.sortspotifyscala.server

import cats.data.Kleisli
import cats.effect.{Async, Resource}
import com.comcast.ip4s.*
import fr.qln.sortspotifyscala.models.Types.implicits.*
import fr.qln.sortspotifyscala.models.Types.{ApplicativeServiceError, HandleDomainError}
import fr.qln.sortspotifyscala.models.config.ServerConfig
import fr.qln.sortspotifyscala.services.{AuthServiceImpl, ConfigServiceImpl}
import fs2.io.net.Network
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.middleware.Logger as Http4sLogger
import org.http4s.{HttpApp, Request, Response}
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object SortSpotifyServer {

  
  def run[F[_] : Async : Network: HandleDomainError: ApplicativeServiceError]: F[Nothing] = {

    implicit val logging: LoggerFactory[F] = Slf4jFactory.create[F]

    for {
      client: Client[F] <- EmberClientBuilder.default[F].build
      serverConfig: ServerConfig <- Resource.eval(new ConfigServiceImpl[F].getServerConfig)
      authService: AuthServiceImpl[F] = new AuthServiceImpl[F](client, serverConfig)

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract segments not checked
      // in the underlying routes.
      httpApp: Kleisli[F, Request[F], Response[F]] = SortSpotifyRoutes.authRoutes[F](authService).orNotFound

      // With Middlewares in place
      finalHttpApp: HttpApp[F] = Http4sLogger.httpApp(true, true)(httpApp)

      _ <-
        EmberServerBuilder.default[F]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(finalHttpApp)
          .build
    } yield ()
  }.useForever


}
