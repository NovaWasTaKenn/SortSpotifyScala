package fr.qln.sortspotifyscala

import cats.data.*
import cats.effect.*
import fr.qln.sortspotifyscala.models.errors.DomainErrors.DomainError
import fr.qln.sortspotifyscala.server.SortSpotifyServer


object Main extends IOApp.Simple:
  type F[A] = EitherT[IO, DomainError, A]
   
  val run: IO[Nothing] = SortSpotifyServer.run[F].rethrowT
