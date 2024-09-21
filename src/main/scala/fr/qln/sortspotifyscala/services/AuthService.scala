package fr.qln.sortspotifyscala.services

import cats.data.EitherT
import cats.effect.kernel.{Concurrent, Ref}
import cats.effect.{Async, Temporal}
import cats.mtl.Raise
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fr.qln.sortspotifyscala.models.AccessTokenResponse
import fr.qln.sortspotifyscala.models.DomainErrors.{AuthenticationError, InvalidReturnedState}
import fr.qln.sortspotifyscala.models.Types.*
import fr.qln.sortspotifyscala.models.config.ServerConfig
import io.circe.generic.auto.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.client.middleware.{Retry, RetryPolicy}
import org.http4s.{Method, Request, Uri, UrlForm}
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import scala.concurrent.duration.{Duration, SECONDS}

case class Tokens(accessToken: String, refreshToken: String)

trait AuthService[F[_]]:

  val storedTokens: F[Ref[F, Tokens]]
  lazy val redirectUrl: String
  
  def refreshTokens: F[Tokens]
  def getInitialTokens(code: String, returnedState: String): F[Tokens]


class AuthServiceImpl[F[_] : Concurrent: Async: Temporal: LoggerFactory: RaiseAuthError: ApplicativeAuthServiceError](client: Client[F], serverConfig: ServerConfig) extends AuthService[F]:

  private lazy val codeVerifier: String = generateRandomString(64)
  private lazy val state: String = generateRandomString(16)

  private val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

  lazy val redirectUrl: String = createRedirectUrl(codeVerifier, serverConfig)
  val storedTokens: F[Ref[F, Tokens]] = Ref[F].of(Tokens("", ""))

  private def createRedirectUrl(codeVerifier: String, serverConfig: ServerConfig): String = s"${serverConfig.authConfig.authUrl}?response_type=code&client_id=${serverConfig.authConfig.clientId}&"
  + s"redirect_uri=${serverConfig.authConfig.callbackUrl}&code_challenge_method=S256&"
  + s"code_challenge=${getCodeChallenge(codeVerifier)}&scope=${URLEncoder.encode(serverConfig.authConfig.scopes, "UTF-8")}&state=$state"




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


  private def getTokens(getTokenRequest: Request[F]): F[Tokens] =

    val retryPolicy: RetryPolicy[F] = RetryPolicy[F](RetryPolicy.exponentialBackoff(maxWait = Duration(5, SECONDS), maxRetry = 5))
    val retryClient: Client[F] = Retry(retryPolicy)(client)

     val tokensEitherT: EitherT[F, Throwable, Tokens] = for {
      response <- retryClient.expect[AccessTokenResponse](getTokenRequest)(jsonOf[F, AccessTokenResponse]).attemptT
      tokens: Tokens =  Tokens(accessToken = response.access_token, refreshToken = response.refresh_token)
    } yield tokens

    tokensEitherT.foldF(
      error => 
        logger.error(error)("Error while trying retrieve tokens") >>
        error.raiseError[F, Tokens], 
      tokens => 
        logger.info("Tokens retrieved successfully") >>
        tokens.pure[F])


  private def updateTokensRef(newTokens: Tokens): F[Tokens] =

    for {
      tokensRef: Ref[F, Tokens] <- storedTokens
      tokensUpdated: Boolean <- tokensRef.tryUpdate(_ => Tokens(newTokens.accessToken, newTokens.refreshToken))
      tokens: Tokens <- tokensRef.get
      tokensUpdated: Tokens <- if tokensUpdated then
        tokens.pure[F]
      else
        new Throwable("Ref update failed").raiseError[F, Tokens]
    } yield tokensUpdated


  private def getAndUpdateTokens(request: Request[F]): F[Tokens] =

    for {
      tokens: Tokens <- getTokens(request)
      updated: Tokens <- updateTokensRef(tokens)
    } yield updated

  private def refreshTokens(serverConfig: ServerConfig): F[Tokens] =

    for {
      _ <- logger.info("Refreshing tokens")
      tokensRef: Ref[F, Tokens] <- storedTokens
      tokens: Tokens <- tokensRef.get
      newTokensRequest = Request[F](
        method = Method.POST,
        uri = Uri.unsafeFromString(serverConfig.authConfig.tokenUrl)
      ).withEntity(
        UrlForm(
          "grant_type" -> "refresh_token",
          "refresh_token" -> tokens.refreshToken,
          "client_id" -> serverConfig.authConfig.clientId,
        )
      )
      refreshedTokens <- getAndUpdateTokens(newTokensRequest)
    } yield refreshedTokens

  def refreshTokens: F[Tokens] = refreshTokens(serverConfig)

  private def getInitialTokens(code: String, returnedState: String, serverConfig: ServerConfig): F[Tokens] =

    val stateCheck: Boolean = state == returnedState

    val authorizationCodeRequest = Request[F](
      method = Method.POST,
      uri = Uri.unsafeFromString(serverConfig.authConfig.tokenUrl)
    ).withEntity(
      UrlForm(
        "grant_type" -> "authorization_code",
        "code" -> code,
        "redirect_uri" -> serverConfig.authConfig.callbackUrl,
        "client_id" -> serverConfig.authConfig.clientId,
        "code_verifier" -> codeVerifier
      )
    )

    for {
      _ <- logger.info("Retrieving initial tokens")
      initialTokens <- if stateCheck then
        getAndUpdateTokens(authorizationCodeRequest)
      else
        Raise[F, AuthenticationError].raise(InvalidReturnedState)
    } yield initialTokens


  def getInitialTokens(code: String, returnedState: String): F[Tokens] = getInitialTokens(code, returnedState, serverConfig)