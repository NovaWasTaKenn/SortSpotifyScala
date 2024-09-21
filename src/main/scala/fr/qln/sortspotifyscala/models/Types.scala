package fr.qln.sortspotifyscala.models

import cats.{Applicative, ApplicativeError}
import cats.mtl.{Handle, Raise}
import fr.qln.sortspotifyscala.models.DomainErrors.*

object Types:

  type RaiseAuthError[F[_]] = Raise[F, AuthenticationError]
  type HandleAuthError[F[_]] = Handle[F, AuthenticationError]
  type HandleDomainError[F[_]] = Handle[F, DomainError]
  type ApplicativeServiceError[F[_]] = ApplicativeError[F, Throwable]
  type RaiseConfigError[F[_]] = Raise[F, ConfigServiceError]
  type ApplicativeConfigServiceError[F[_]] = ApplicativeError[F, Throwable]
  type ApplicativeAuthServiceError[F[_]] = ApplicativeError[F, Throwable]
  

  object implicits: 

    implicit def handleAuthenticationError[F[_]](implicit handleDomainError: Handle[F, DomainError]): Handle[F, AuthenticationError] =
      new Handle[F, AuthenticationError] {
        override def handleWith[A](fa: F[A])(f: AuthenticationError => F[A]): F[A] =
          handleDomainError.handleWith(fa) {
            case authError: AuthenticationError => f(authError)
            case otherError => handleDomainError.raise(otherError)
          }
    
        override def raise[E2 <: AuthenticationError, A](e: E2): F[A] = 
          handleDomainError.raise(e.asInstanceOf[DomainError])

        override def applicative: Applicative[F] = handleDomainError.applicative
      }
    