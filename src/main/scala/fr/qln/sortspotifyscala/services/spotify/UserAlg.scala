package fr.qln.sortspotifyscala.services.spotify

import cats.Monad
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.spotifyTypes.{PaginatedResponse, SavedTrackItem, UserProfile}
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.RaiseSessionError
import fr.qln.sortspotifyscala.services.auth.SessionId
import fr.qln.sortspotifyscala.services.utils.ClientService
import io.circe.generic.auto.deriveDecoder
import org.http4s.Headers
import org.typelevel.log4cats.LoggerFactory

trait UserAlg[F[_]] {
  def getSpotifyProfile(sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[UserProfile]
  def getSavedTracks(sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[Seq[SavedTrackItem]]
}
object UserAlg :
  def create[F[_]: Monad: LoggerFactory](clientService : ClientService[F]): UserAlg[F] = new UserAlg[F] {

    private val logger = LoggerFactory[F].getLogger

    def getSpotifyProfile(sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[UserProfile] =
      
      for {
        _ <- logger.debug("Retrieving user profile")
        userProfile <- clientService.getSpotifyAuthed[UserProfile](
          url = "https://api.spotify.com/v1/me",
          sessionId = sessionId,
          headers = Headers.empty
        )
        _ <- logger.debug(s"user profile: $userProfile")
      } yield userProfile

    def getSavedTracks(sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[Seq[SavedTrackItem]] =

      def getSavedTracksBatch(sessionId: SessionId, limit: Int, offset: Int): F[PaginatedResponse[SavedTrackItem]] = {
        for {
          _ <- logger.debug("Retrieving user saved tracks")
          response <- clientService.getSpotifyAuthed[PaginatedResponse[SavedTrackItem]](
            url = s"https://api.spotify.com/v1/me/tracks?offset=$offset&limit=$limit",
            sessionId = sessionId,
            headers = Headers.empty
          )
          _ <- logger.debug(s"user profile: $response")
        } yield response
      }

      val getSavedTracksBatchPartial: (Int, Int)=>F[PaginatedResponse[SavedTrackItem]]=getSavedTracksBatch(sessionId, _, _)

      clientService.requestWithPaginatedResponse[SavedTrackItem](getSavedTracksBatchPartial, 50)

  }
