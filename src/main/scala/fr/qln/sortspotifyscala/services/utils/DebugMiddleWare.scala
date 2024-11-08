package fr.qln.sortspotifyscala.services.utils

import cats.data.{Kleisli, OptionT}
import cats.effect.Async
import cats.syntax.all.*
import org.http4s.HttpRoutes
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}

trait DebugMiddleware[F[_]] {
  def debugMiddleware(service: HttpRoutes[F]): HttpRoutes[F]

}

class DebugMiddlewareImpl[F[_]: Async : LoggerFactory] extends DebugMiddleware[F]:

  val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

  def debugMiddleware(service: HttpRoutes[F]): HttpRoutes[F]=
    Kleisli { req =>
      OptionT(
        for {
          _ <- logger.debug(s"Incoming request: ${req.toString}")
          response <- service(req).value
          _ <- logger.debug(s"Outgoing response: ${response.toString}")
        } yield response
      )
    }
