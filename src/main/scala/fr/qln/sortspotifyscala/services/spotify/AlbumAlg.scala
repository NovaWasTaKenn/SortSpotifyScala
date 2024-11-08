package fr.qln.sortspotifyscala.services.spotify

import cats.Monad
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.spotifyTypes.{Album, PaginatedResponse}
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.RaiseSessionError
import fr.qln.sortspotifyscala.services.auth.SessionId
import fr.qln.sortspotifyscala.services.utils.ClientService
import io.circe.generic.auto.deriveDecoder
import org.http4s.Headers

type AlbumResponseType = Option[Album]

trait AlbumAlg[F[_]] {
  def getSeveralAlbums(sessionId: SessionId, ids: Seq[String])(using RaiseSessionError[F]): F[Seq[AlbumResponseType]]
}
object AlbumAlg {

  def create[F[_]: Monad](clientService : ClientService[F]): AlbumAlg[F] = new AlbumAlg[F] {

    
    def getSeveralAlbums(sessionId: SessionId, albumIds: Seq[String])(using RaiseSessionError[F]): F[Seq[AlbumResponseType]] = {

      def getSeveralAlbumBatch(_sessionId: SessionId, albumIds: Seq[String])(using RaiseSessionError[F]): F[Seq[AlbumResponseType]] = {
        for {
          response <- clientService.getSpotifyAuthed[Seq[AlbumResponseType]](
            s"https://api.spotify.com/v1/albums?ids=${albumIds.mkString(",")}",
            sessionId = _sessionId,
            headers = Headers.empty
          )
        } yield response
      }
      
      
      val getSeveralAlbumBatchPartial: Seq[String] => F[Seq[AlbumResponseType]] = getSeveralAlbumBatch(sessionId, _)
      val groupResults: Iterator[(Int, F[Seq[AlbumResponseType]])]=>F[Seq[AlbumResponseType]] = results => results.map(_._2).toList.sequence.map(_.flatten)

      clientService.batchedRequests[Seq[AlbumResponseType], Seq[AlbumResponseType], String](20, albumIds, getSeveralAlbumBatchPartial, groupResults)
      
    }


  }


}