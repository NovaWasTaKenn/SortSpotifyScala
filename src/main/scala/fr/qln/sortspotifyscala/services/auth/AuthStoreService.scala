package fr.qln.sortspotifyscala.services.auth

import cats.effect.Async
import cats.mtl.Raise
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fr.qln.sortspotifyscala.models.errors.DomainErrors.{AuthenticationError, AuthenticationValuesAlreadyExists, AuthenticationValuesNotFound}
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.{ApplicativeAuthServiceError, RaiseAuthError}
import org.typelevel.log4cats.LoggerFactory

import scala.collection.concurrent.TrieMap

trait AuthStoreService[F[_]]:

   val getAuthValues: SessionId => F[AuthValues]
   val removeAuthValues: SessionId => F[Boolean]
   val addAuthValues: AuthValues => F[Boolean]
   val exists: SessionId => F[Boolean]

class AuthStoreServiceImpl[F[_]: Async: LoggerFactory: RaiseAuthError: ApplicativeAuthServiceError] extends AuthStoreService[F]:

  private val authStore: TrieMap[SessionId, AuthValues] = TrieMap.empty

  val getAuthValues: SessionId => F[AuthValues] = getAuthValues(_, authStore)
  val removeAuthValues: SessionId => F[Boolean] = removeAuthValues(_, authStore)
  val addAuthValues: AuthValues => F[Boolean] = addAuthValues(_, authStore)
  val exists: SessionId => F[Boolean] = exists(_, authStore)


  private def getAuthValues(authId: SessionId, authStore: TrieMap[SessionId, AuthValues]): F[AuthValues] =
    optionToAuthValuesNotFound(authStore.get(authId))

  private def removeAuthValues(authId: SessionId, authStore: TrieMap[SessionId, AuthValues]): F[Boolean] =
    optionToAuthValuesNotFound(authStore.remove(authId)).map(_=>true)

  private def addAuthValues(authValues: AuthValues, authStore: TrieMap[SessionId, AuthValues]): F[Boolean] =
    for {
      authValuesOption <- authStore.put(authValues.sessionId, authValues).pure[F]
      authValuesSuccessfullyAdded <- authValuesOption match {
        case Some(_) => Raise[F, AuthenticationError].raise(AuthenticationValuesAlreadyExists)
        case None => true.pure[F]
      }
    } yield authValuesSuccessfullyAdded

  private def exists(authId: SessionId, authValuesStore: TrieMap[SessionId, AuthValues]): F[Boolean] = 
    authValuesStore.exists(_._1==authId).pure[F]


  private def optionToAuthValuesNotFound(option: Option[AuthValues]): F[AuthValues] =

    for {
      authValuesOption: Option[AuthValues] <- option.pure[F]
      authValues <- authValuesOption match {
        case Some(authValues) => authValues.pure[F]
        case None => Raise[F, AuthenticationError].raise(AuthenticationValuesNotFound)
      }
    } yield authValues