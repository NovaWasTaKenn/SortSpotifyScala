package fr.qln.sortspotifyscala.services.spotify

import cats.Monad
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.spotifyTypes.*
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.RaiseSessionError
import fr.qln.sortspotifyscala.services.auth.{SessionId, SessionService}
import fr.qln.sortspotifyscala.services.utils.ClientService
import io.circe.generic.auto.deriveDecoder
import org.http4s.{Headers, UrlForm}

trait PlaylistAlg[F[_]] {
  
    def getPlaylist(playlistId: String, sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[Playlist]
    def createSeveralPlaylists(sessionId: SessionId, createPlaylistBodies: Seq[CreatePlaylistBody])(implicit raiseSessionError: RaiseSessionError[F]): F[Seq[Playlist]]
    def getUserPlaylists(sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[Seq[Playlist]]

    def addItems(playlistId: String, itemsUris: Seq[String], sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[String]
    def updatePlaylist(playlistId: String, itemsUris: Seq[String], sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[String]
    def getOrCreateSeveralPlaylists(sessionId: SessionId, createPlaylistBodies: Seq[CreatePlaylistBody])(implicit raiseSessionError: RaiseSessionError[F]): F[(Seq[Playlist], Seq[Playlist])]
}

object PlaylistAlg {

  def create[F[_]: Monad](sessionService: SessionService[F], clientService : ClientService[F]): PlaylistAlg[F] =  new PlaylistAlg[F] {

    def getOrCreateSeveralPlaylists(sessionId: SessionId, createPlaylistBodies: Seq[CreatePlaylistBody])
                                            (implicit raiseSessionError: RaiseSessionError[F]): F[(Seq[Playlist], Seq[Playlist])] =

      for {
        userPlaylists <- getUserPlaylists(sessionId)
        existingPlaylists = userPlaylists.filter(playlist => createPlaylistBodies.exists(_.name.toLowerCase == playlist.name.toLowerCase))
        playlistsToCreate = createPlaylistBodies.filter(body => !userPlaylists.exists(_.name.toLowerCase == body.name.toLowerCase))
        createdPlaylists <- createSeveralPlaylists(sessionId, playlistsToCreate)
      } yield (existingPlaylists, createdPlaylists)

    def getPlaylist(playlistId: String, sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[Playlist] = {
      for {
        playlist <- clientService.getSpotifyAuthed[Playlist](s"https://api.spotify.com/v1/playlists/$playlistId", sessionId = sessionId, headers = Headers.empty)
      } yield playlist
    }

    def createSeveralPlaylists(sessionId: SessionId, createPlaylistBodies: Seq[CreatePlaylistBody])(implicit raiseSessionError: RaiseSessionError[F]): F[Seq[Playlist]] =

      def createPlaylist(sessionId: SessionId, createPlaylistBodies: Seq[CreatePlaylistBody])(implicit raiseSessionError: RaiseSessionError[F]): F[Playlist] =
        for {
          session <- sessionService.getStoredSession(sessionId)
          newPlaylist <- clientService.postSpotifyAuthed[Playlist](
            s"https://api.spotify.com/v1/users/${session.spotifyUserId}/playlists",
            sessionId = sessionId,
            headers = Headers.empty,
            body = UrlForm(
              "name" -> createPlaylistBodies.head.name,
              "public" -> createPlaylistBodies.head.public.toString,
              "collaborative" -> createPlaylistBodies.head.collaborative.toString,
              "description" -> createPlaylistBodies.head.description,
            )
          )
        } yield newPlaylist


      val createPlaylistsBatchPartial:  Seq[CreatePlaylistBody] => F[Playlist] = createPlaylist(sessionId, _)
      val groupResults: Iterator[(Int, F[Playlist])] => F[Seq[Playlist]] = (results: Iterator[(Int, F[Playlist])]) => results.map(_._2).toList.sequence.map(_.toSeq)

      clientService.batchedRequests[Playlist,Seq[Playlist], CreatePlaylistBody](1, createPlaylistBodies, createPlaylistsBatchPartial, groupResults)

    def getUserPlaylists(sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[Seq[Playlist]] =

      def getUserPlaylistsBatch(sessionId: SessionId, limit: Int, offset: Int)(implicit raiseSessionError: RaiseSessionError[F]): F[PaginatedResponse[Playlist]] = {
        for {
          session <- sessionService.getStoredSession(sessionId)
          response <- clientService.getSpotifyAuthed[PaginatedResponse[Playlist]](
            s"https://api.spotify.com/v1/users/${session.spotifyUserId}/playlists?offset=$offset&limit=$limit",
            sessionId = sessionId,
            headers = Headers.empty
          )
        } yield response
      }
      
      val getUserPlaylistsBatchPartial: (Int, Int)=>F[PaginatedResponse[Playlist]] = getUserPlaylistsBatch(sessionId, _, _)

      clientService.requestWithPaginatedResponse[Playlist](getUserPlaylistsBatchPartial, 50)
    

    def addItems(playlistId: String, itemsUris: Seq[String], _sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[String] =

      def addItemsBatch(playlistId: String, itemsUris: Seq[String], _sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[String] =
        for {
          response <- clientService.postSpotifyAuthed[AddItemsResponse](
            url = s"https://api.spotify.com/v1/playlists/$playlistId/tracks",
            sessionId = _sessionId,
            body = UrlForm(
              "uris" -> itemsUris.mkString(",")
            ),
            headers = Headers.empty
          )
          snapshotId = response.snapshot_id
        } yield snapshotId
      
      
      
      val addItemsBatchPartial: Seq[String] => F[String] = addItemsBatch(playlistId, _, _sessionId)
      val groupResults = (results: Iterator[(Int, F[String])]) => results.maxBy(_._1)._2

      clientService.batchedRequests[String, String, String](100, itemsUris, addItemsBatchPartial, groupResults)
    

    def updatePlaylist(playlistId: String, itemsUris: Seq[String], _sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[String] =

      def updatePlaylistBatch(playlistId: String, itemsUris: Seq[String], _sessionId: SessionId)(implicit raiseSessionError: RaiseSessionError[F]): F[String] = {
        for {
          response <- clientService.putSpotifyAuthed[AddItemsResponse](
            url = s"https://api.spotify.com/v1/playlists/$playlistId/tracks",
            sessionId = _sessionId,
            body = UrlForm(
              "uris" -> itemsUris.mkString(",")
            ),
            headers = Headers.empty
          )
          snapshotId = response.snapshot_id
        } yield snapshotId
      }


      val updatePlaylistBatchPartial: Seq[String] => F[String] = updatePlaylistBatch(playlistId, _, _sessionId)
      val groupResults = (results: Iterator[(Int, F[String])]) => results.maxBy(_._1)._2

      clientService.batchedRequests[String, String, String](100, itemsUris, updatePlaylistBatchPartial, groupResults)

    
      

  }

}