package fr.qln.sortspotifyscala.services.utils

import cats.effect.{Async, Resource}
import cats.mtl.Raise
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.config.ClientConfig
import fr.qln.sortspotifyscala.models.errors.DomainErrors.{SessionError, SessionNotAuthenticated}
import fr.qln.sortspotifyscala.models.spotifyTypes.PaginatedResponse
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.RaiseSessionError
import fr.qln.sortspotifyscala.services.auth.{SessionId, SessionService}
import fs2.io.net.Network
import io.circe.Decoder
import org.http4s.circe.jsonOf
import org.http4s.client.Client
import org.http4s.client.middleware.{Retry, RetryPolicy}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import org.http4s.{AuthScheme, Credentials, Header, Headers, Method, Request, Uri, UrlForm}
import org.typelevel.log4cats.LoggerFactory

import scala.reflect.ClassTag

trait ClientService[F[_]]:
  def createClient(clientConfig: ClientConfig): Resource[F, Client[F]]

  def sendSpotify[RetrievedObjectType: ClassTag : Decoder](request: Request[F]): F[RetrievedObjectType]

  def postSpotify[RetrievedObjectType: ClassTag : Decoder]
  (url: String, body: UrlForm, headers: Headers)
  (implicit raiseSessionError: RaiseSessionError[F]): F[RetrievedObjectType]


  def sendSpotifyAuthed[RetrievedObjectType: ClassTag : Decoder](sessionId: SessionId, request: Request[F])(implicit raiseSessionError: Raise[F, SessionError]): F[RetrievedObjectType]

  def getSpotifyAuthed[RetrievedObjectType: ClassTag : Decoder]
  (url: String, sessionId: SessionId, headers: Headers)
  (implicit raiseSessionError: Raise[F, SessionError]): F[RetrievedObjectType]

  def postSpotifyAuthed[RetrievedObjectType: ClassTag : Decoder]
  (url: String, sessionId: SessionId, body: UrlForm, headers: Headers)
  (implicit raiseSessionError: Raise[F, SessionError]): F[RetrievedObjectType]

  def putSpotifyAuthed[RetrievedObjectType: ClassTag : Decoder]
  (url: String, sessionId: SessionId, body: UrlForm, headers: Headers)
  (implicit raiseSessionError: Raise[F, SessionError]): F[RetrievedObjectType]

  def batchedRequests[BatchResponseType, FinalResponseType, ItemType](
                                                                       batchSize: Int,
                                                                       items: Seq[ItemType],
                                                                       batchRequestFunc: Seq[ItemType] => F[BatchResponseType],
                                                                       groupBatchResponsesFunc: Iterator[(Int, F[BatchResponseType])] => F[FinalResponseType])
                                                                     (implicit raiseSessionError: RaiseSessionError[F]): F[FinalResponseType]

  def requestWithPaginatedResponse[ItemType](requestFunc: (Int, Int) => F[PaginatedResponse[ItemType]], limit: Int = 50, offset: Int = 0)
                                            (implicit raiseSessionError: RaiseSessionError[F]): F[Seq[ItemType]]

object ClientService:
  def create[F[_] : Async : Network :  LoggerFactory](sessionService: SessionService[F], clientConfig: ClientConfig): ClientService[F] = new ClientService[F] {

    private val logger = LoggerFactory[F].getLogger


    def createClient(clientConfig: ClientConfig): Resource[F, Client[F]] =
      for {
        client: Client[F] <- EmberClientBuilder.default[F].build
        retryPolicy: RetryPolicy[F] = RetryPolicy[F](RetryPolicy.exponentialBackoff(maxWait = clientConfig.retryMaxWait, maxRetry = clientConfig.retryMaxRetries))
        retryClient: Client[F] = Retry(retryPolicy)(client)

        finalClient = retryClient
      } yield finalClient

    val client: Resource[F, Client[F]] = createClient(clientConfig)

    def sendSpotify[RetrievedObjectType: ClassTag : Decoder](request: Request[F]): F[RetrievedObjectType] =

      val classTag = implicitly[ClassTag[RetrievedObjectType]]
      val retrievedObjectName = classTag.runtimeClass.getName

      for {
        _ <- logger.debug(s"Retrieve $retrievedObjectName request: $request")
        retrievedObject <- client.use(client => client.expect[RetrievedObjectType](request)(jsonOf[F, RetrievedObjectType])).attemptT
          .foldF(
            error =>
              logger.error(error)(s"Error while trying retrieve $retrievedObjectName") >>
                error.raiseError[F, RetrievedObjectType],
            retrievedObject =>
              logger.debug(s"$retrievedObjectName retrieved successfully") >>
                retrievedObject.pure[F]
          )
      } yield retrievedObject


    def sendSpotifyAuthed[RetrievedObjectType: ClassTag : Decoder]
    (sessionId: SessionId, request: Request[F])
    (implicit raiseSessionError: Raise[F, SessionError]): F[RetrievedObjectType] =


      for {
        _ <- logger.debug(s"Adding authentication header to request: $request")
        storedSession <- sessionService.getStoredSession(sessionId)
        _ <- storedSession.authed match {
          case true => ().pure[F]
          case false => raiseSessionError.raise(SessionNotAuthenticated)
        }
        token = storedSession.tokens.get.accessToken

        requestWithToken = request.withHeaders(
          Authorization(Credentials.Token(AuthScheme.Bearer, token)) //Header.Raw(CIString("Authorization"), s"Bearer $token")
        )

        retrievedObject <- sendSpotify[RetrievedObjectType](requestWithToken)
      } yield retrievedObject


    def getSpotifyAuthed[RetrievedObjectType: ClassTag : Decoder]
    (url: String, sessionId: SessionId, headers: Headers)
    (implicit raiseSessionError: Raise[F, SessionError]): F[RetrievedObjectType] =

      val request = Request[F](
        method = Method.GET,
        uri = Uri.unsafeFromString(url)
      ).withHeaders(headers)

      sendSpotifyAuthed[RetrievedObjectType](sessionId, request)

    def postSpotifyAuthed[RetrievedObjectType: ClassTag : Decoder]
    (url: String, sessionId: SessionId, body: UrlForm, headers: Headers)
    (implicit raiseSessionError: Raise[F, SessionError]): F[RetrievedObjectType] =

      val request = Request[F](
        method = Method.POST,
        uri = Uri.unsafeFromString(url)
      ).withEntity(body)
        .withHeaders(headers)

      sendSpotifyAuthed[RetrievedObjectType](sessionId, request)


    def putSpotifyAuthed[RetrievedObjectType: ClassTag : Decoder]
    (url: String, sessionId: SessionId, body: UrlForm, headers: Headers)
    (implicit raiseSessionError: Raise[F, SessionError]): F[RetrievedObjectType] =

      val request = Request[F](
        method = Method.PUT,
        uri = Uri.unsafeFromString(url)
      ).withEntity(body)
        .withHeaders(headers)

      sendSpotifyAuthed[RetrievedObjectType](sessionId, request)


    def postSpotify[RetrievedObjectType: ClassTag : Decoder]
    (url: String, body: UrlForm, headers: Headers)
    (implicit raiseSessionError: RaiseSessionError[F]): F[RetrievedObjectType] =

      for {
        _ <- logger.debug(s"""postSpotify params, url: $url, body: $body, headers: $headers""")

        request = Request[F](
          method = Method.POST,
          uri = Uri.unsafeFromString(url),
          headers = headers
        ).withEntity(body)

        _ <- logger.debug(s"postSpotify request: $request")

        response <- sendSpotify[RetrievedObjectType](request)

      } yield response


    def requestWithPaginatedResponse[ItemType](requestFunc: (Int, Int) => F[PaginatedResponse[ItemType]], limit: Int, offset: Int = 0)(
      implicit raiseSessionError: RaiseSessionError[F]): F[Seq[ItemType]] =

      for {
        response: PaginatedResponse[ItemType] <- requestFunc(limit, offset)
        intermediateResults: Seq[ItemType] <- if (response.next.isDefined) requestWithPaginatedResponse[ItemType](requestFunc, limit, offset = offset + limit) else Seq().pure[F]
        results: Seq[ItemType] = response.items ++ intermediateResults
      } yield results

    def batchedRequests[BatchResponseType, FinalResponseType, ItemType]
    (
      batchSize: Int,
      items: Seq[ItemType],
      batchRequestFunc: Seq[ItemType] => F[BatchResponseType],
      groupBatchResponsesFunc: Iterator[(Int, F[BatchResponseType])] => F[FinalResponseType])
    (implicit raiseSessionError: RaiseSessionError[F]): F[FinalResponseType] =
    
      groupBatchResponsesFunc(
        items
          .grouped(batchSize)
          .zipWithIndex
          .map((batch, batchIndex) =>
            (batchIndex, batchRequestFunc(batch))
          )
      )


  }

