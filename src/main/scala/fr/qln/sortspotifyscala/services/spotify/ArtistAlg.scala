package fr.qln.sortspotifyscala.services.spotify

import cats.Monad
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.spotifyTypes.Artist
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.RaiseSessionError
import fr.qln.sortspotifyscala.services.auth.SessionId
import fr.qln.sortspotifyscala.services.utils.ClientService
import io.circe.generic.auto.deriveDecoder
import org.http4s.Headers

type ArtistResponseType = Option[Artist]

trait ArtistAlg[F[_]] {
  def getSeveralArtist(sessionId: SessionId, ids: Seq[String])(using RaiseSessionError[F]): F[Seq[ArtistResponseType]]
}
object ArtistAlg {

  def create[F[_]: Monad](clientService : ClientService[F]): ArtistAlg[F] = new ArtistAlg[F] {

    def getSeveralArtist(sessionId: SessionId, artistIds: Seq[String])(using RaiseSessionError[F]): F[Seq[ArtistResponseType]] = {

      def getSeveralArtistBatch(_sessionId: SessionId, artistIds: Seq[String])(using RaiseSessionError[F]): F[Seq[ArtistResponseType]] = {
        for {
          response <- clientService.getSpotifyAuthed[Seq[ArtistResponseType]](
            s"https://api.spotify.com/v1/albums?ids=${artistIds.mkString(",")}",
            sessionId = _sessionId,
            headers = Headers.empty
          )
        } yield response
      }

      val getSeveralArtistBatchPartial: Seq[String] => F[Seq[ArtistResponseType]] = getSeveralArtistBatch(sessionId, _)
      val groupResults: Iterator[(Int, F[Seq[ArtistResponseType]])]=>F[Seq[ArtistResponseType]] = results => results.map(_._2).toList.sequence.map(_.flatten)

      clientService.batchedRequests[Seq[ArtistResponseType], Seq[ArtistResponseType], String](20, artistIds, getSeveralArtistBatchPartial, groupResults)

    }
    
  }


}
