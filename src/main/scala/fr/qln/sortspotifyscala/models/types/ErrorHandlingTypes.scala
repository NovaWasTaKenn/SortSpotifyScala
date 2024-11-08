package fr.qln.sortspotifyscala.models.types

import cats.mtl.{Handle, Raise}
import cats.{Applicative, ApplicativeError}
import fr.qln.sortspotifyscala.models.errors.DomainErrors.*
import scala.reflect.ClassTag

object ErrorHandlingTypes:

  type RaiseAuthError[F[_]] = Raise[F, AuthenticationError]
  type HandleAuthError[F[_]] = Handle[F, AuthenticationError]
  type RaiseConfigError[F[_]] = Raise[F, ConfigError]

  type ApplicativeConfigServiceError[F[_]] = ApplicativeError[F, Throwable]
  type ApplicativeAuthServiceError[F[_]] = ApplicativeError[F, Throwable]
  type ApplicativeSessionServiceError[F[_]] = ApplicativeError[F, Throwable]
  type ApplicativeSortServiceError[F[_]] = ApplicativeError[F, Throwable]

  
  type RaiseSessionError[F[_]] = Raise[F, SessionError]
  type HandleSessionError[F[_]] = Handle[F, SessionError]
  
  
  type RaiseSortError[F[_]] = Raise[F, SortError]
  type HandleSortError[F[_]] = Handle[F, SortError]


  type ApplicativeServiceError[F[_]] = ApplicativeError[F, Throwable]
  type HandleDomainError[F[_]] = Handle[F, DomainError]

  
  
  object implicits: 

    implicit def handleChildError[F[_], E <: DomainError](implicit handleDomainError: Handle[F, DomainError], ct: ClassTag[E]): Handle[F, E] =
      new Handle[F, E] {
        override def handleWith[A](fa: F[A])(f: E => F[A]): F[A] =
          handleDomainError.handleWith(fa) {
            case sessionError if ct.runtimeClass.isInstance(sessionError) => f(sessionError.asInstanceOf[E])
            case otherError => handleDomainError.raise(otherError)
          }

        override def raise[E2 <: E, A](e: E2): F[A] =
          handleDomainError.raise(e.asInstanceOf[DomainError])

        override def applicative: Applicative[F] = handleDomainError.applicative
      }