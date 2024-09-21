package fr.qln.sortspotifyscala.services

import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import ciris.*
import fr.qln.sortspotifyscala.models.Types.*
import fr.qln.sortspotifyscala.models.config.Environments.AppEnvironment
import fr.qln.sortspotifyscala.models.config.Environments.AppEnvironment.{Local, Production, Testing}
import fr.qln.sortspotifyscala.models.config.{AuthConfig, ServerConfig}
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}

import java.nio.file.Path




trait ConfigService[F[_]]:
  def getServerConfig: F[ServerConfig]


class ConfigServiceImpl[F[_] : Async: LoggerFactory: RaiseConfigError: ApplicativeConfigServiceError] extends ConfigService[F]:
  private val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

  override def getServerConfig: F[ServerConfig] =
    val serverConfig: ConfigValue[F, ServerConfig] = for {
      configDir <- env("CONFIG_DIR").default("")
      appEnvironment <- env("APP_ENVIRONMENT").default("testing").as[AppEnvironment]
      authConfig: AuthConfig <- ConfigValue.eval(getAuthConfig(configDir, appEnvironment).map(default)) 
      serverConfig: ServerConfig = ServerConfig(authConfig = authConfig)
    } yield serverConfig

    serverConfig.attempt[F]
      .flatMap {
        case Left(configError) => configError.throwable.raiseError[F, ServerConfig]
        case Right(value) => value.pure[F]
      }
      .flatTap(serverConfig => logger.info(s"Server config: $serverConfig"))

  private def getAuthConfig(configDir: String, appEnvironment: AppEnvironment): F[AuthConfig] =

    for {
      _ <- logger.info(s"getting auth config from path: $configDir/authConfig.yml and environment: $appEnvironment")
      attemptConfig: Either[ConfigError, AuthConfig] <- appEnvironment match {
        case Local => file(
          Path.of(configDir + "/authConfig.yml")
        ).as[AuthConfig]
          .attempt[F]
        case Testing | Production => file(
          Path.of(configDir + "/authConfig.yml")
        ).as[AuthConfig]
          .attempt[F]

      }

      configValue <- (attemptConfig, appEnvironment) match {
        case (Right(value), _) =>
          logger.info("config retrieved successfully") >>
          Async[F].pure(value)
        case (Left(error), Local) =>
          logger.error(error.throwable)("Error retrieving config for local environment. Using default config") >>
          Async[F].pure(AuthConfig(
            clientId = "09f86191c869489b84914d1172051c61",
            scopes = "user-read-private user-read-email",
            callbackUrl = "http://localhost:8080/callback",
            authUrl = "https://accounts.spotify.com/authorize",
            tokenUrl = "https://accounts.spotify.com/api/token",
            homeUrl = "http://localhost:8080/home"
          ))
        case (Left(error), Testing | Production) =>
          logger.error(error.throwable)(s"Error retrieving config for $appEnvironment environment. Using default config") >>
          Async[F].pure(AuthConfig(
            clientId = "09f86191c869489b84914d1172051c61",
            scopes = "user-read-private user-read-email",
            callbackUrl = "http://localhost:8080/callback",
            authUrl = "https://accounts.spotify.com/authorize",
            tokenUrl = "https://accounts.spotify.com/api/token",
            homeUrl = "http://localhost:8080/home"
          ))
      }
    } yield configValue



    




