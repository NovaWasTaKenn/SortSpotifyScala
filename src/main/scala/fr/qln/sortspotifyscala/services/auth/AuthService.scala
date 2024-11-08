package fr.qln.sortspotifyscala.services.auth

import cats.effect.Async
import cats.effect.kernel.Concurrent
import cats.mtl.Raise
import cats.mtl.syntax.all.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fr.qln.sortspotifyscala.models.config.ServerConfig
import fr.qln.sortspotifyscala.models.errors.DomainErrors.{AuthenticationError, InvalidReturnedState, TokensNotDefined}
import fr.qln.sortspotifyscala.models.spotifyTypes.AccessToken
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.*
import fr.qln.sortspotifyscala.services.*
import fr.qln.sortspotifyscala.services.utils.ClientService
import io.circe.generic.auto.*
import org.http4s.{Headers, UrlForm}
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, SecureRandom}
import java.util.Base64


case class Tokens(accessToken: String, refreshToken: String)

case class AuthValues(sessionId: SessionId, codeVerifier: String, state: String)

trait AuthService[F[_]]:

  def initAuthValues(sessionId: SessionId): F[AuthValues]

  def createRedirectUrl(codeVerifier: String, state: String): String

  def refreshTokens(sessionId: SessionId): F[Tokens]

  def getInitialTokens(authId: SessionId, code: String, returnedState: String): F[Tokens]


class AuthServiceImpl[F[_] : Concurrent : Async : LoggerFactory : RaiseAuthError : ApplicativeAuthServiceError : RaiseSessionError]
(clientService: ClientService[F], authStore: AuthStoreService[F], sessionService: SessionService[F], serverConfig: ServerConfig) extends AuthService[F]:


  private val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

  private def initAuthValues(sessionId: SessionId, authStore: AuthStoreService[F]): F[AuthValues] =

    for {
      _ <- logger.info(s"Initializing auth values")
      codeVerifier = generateRandomString(64)
      state = generateRandomString(16)
      authValues = AuthValues(sessionId, codeVerifier, state)
      authValuesAdded <- authStore.addAuthValues(authValues)
      storedValues <- authStore.getAuthValues(authValues.sessionId)
      _ <- logger.debug(s"Stored auth values: $storedValues")
    } yield authValues


  def initAuthValues(sessionId: SessionId): F[AuthValues] = initAuthValues(sessionId, authStore)

  private def createRedirectUrl(codeVerifier: String, state: String, serverConfig: ServerConfig): String =s"${serverConfig.authConfig.authUrl}?response_type=code&client_id=${serverConfig.authConfig.clientId}&"+
    s"redirect_uri=${serverConfig.authConfig.callbackUrl}&code_challenge_method=S256&"+
    s"code_challenge=${getCodeChallenge(codeVerifier)}&scope=${URLEncoder.encode(serverConfig.authConfig.scopes, "UTF-8")}&state=$state"

  def createRedirectUrl(codeVerifier: String, state: String): String = createRedirectUrl(codeVerifier, state, serverConfig)

  private def generateRandomString(length: Int, possible: String, random: SecureRandom): String =
    val values = new Array[Byte](length)
    random.nextBytes(values)
    values.map(b => possible((b & 0xFF) % possible.length)).mkString

  private val generateRandomString: Int => String = generateRandomString(_,
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    new SecureRandom()
  )


  private def sha256(plain: String): Array[Byte] =
    val digest = MessageDigest.getInstance("SHA-256")
    val encodedData = plain.getBytes(StandardCharsets.UTF_8)
    digest.digest(encodedData)


  private def base64encode(input: Array[Byte]): String =
    val encoded = Base64.getEncoder.encodeToString(input)
    encoded.replace("=", "")
      .replace("+", "-")
      .replace("/", "_")


  private def getCodeChallenge(codeVerifier: String): String =
    val hashed: Array[Byte] = sha256(codeVerifier)
    base64encode(hashed)

  private def refreshTokens(sessionId: SessionId, serverConfig: ServerConfig, sessionService: SessionService[F]): F[Tokens] =

    for {
      _ <- logger.info("Refreshing tokens")
      session <- sessionService.getStoredSession(sessionId)
      tokens: Tokens <- session.tokens match {
        case Some(values) => values.pure[F]
        case None => TokensNotDefined.raise[F, Tokens]
      }

      tokensResponse <- clientService.postSpotify[AccessToken](
        url = serverConfig.authConfig.tokenUrl,
        body = UrlForm(
          "grant_type" -> "refresh_token",
          "refresh_token" -> tokens.refreshToken,
          "client_id" -> serverConfig.authConfig.clientId,
        ),
        headers = Headers.empty
      )
      refreshedTokens = Tokens(tokensResponse.access_token, tokensResponse.refresh_token)
      _ <- sessionService.modifySession(session.copy(tokens = Some(refreshedTokens)))
    } yield refreshedTokens

  def refreshTokens(sessionId: SessionId): F[Tokens] = refreshTokens(sessionId, serverConfig, sessionService)

  private def getInitialTokens(sessionId: SessionId, code: String, returnedState: String, serverConfig: ServerConfig, authStore: AuthStoreService[F]): F[Tokens] =

    for {
      authValues <- authStore.getAuthValues(sessionId)
      stateChecked <- Raise[F, AuthenticationError].ensure(authValues.state.pure[F])(InvalidReturnedState)(state => state == returnedState)
      
      _ <- logger.debug("Retrieving initial tokens")

      tokensResponse <- clientService.postSpotify[AccessToken](
        url = serverConfig.authConfig.tokenUrl,
        body = UrlForm(
          "grant_type" -> "authorization_code",
          "code" -> code,
          "redirect_uri" -> serverConfig.authConfig.callbackUrl,
          "client_id" -> serverConfig.authConfig.clientId,
          "code_verifier" -> authValues.codeVerifier
        ),
        headers = Headers.empty
      )
      
      initialTokens = Tokens(tokensResponse.access_token, tokensResponse.refresh_token)
      
    } yield initialTokens


  def getInitialTokens(authId: SessionId, code: String, returnedState: String): F[Tokens] =
    getInitialTokens(authId, code, returnedState, serverConfig, authStore)


