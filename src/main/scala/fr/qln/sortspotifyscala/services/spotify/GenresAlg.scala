package fr.qln.sortspotifyscala.services.spotify

import cats.Monad
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.spotifyTypes.{Track, TrackEnriched}
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.RaiseSessionError
import fr.qln.sortspotifyscala.services.auth.SessionId
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}

trait GenresAlg[F[_]] {
  def getSeveralAlbumGenres(sessionId: SessionId, albumIds: Seq[String])(implicit raiseSessionError: RaiseSessionError[F]): F[Map[String, Seq[String]]]
  def getSeveralArtistGenres(sessionId: SessionId, artistIds: Seq[String])(implicit raiseSessionError: RaiseSessionError[F]): F[Map[String, Seq[String]]]
  def getSeveralTracksGenres(sessionId: SessionId, tracks: Seq[Track])
                            (implicit raiseSessionError: RaiseSessionError[F]): F[Seq[TrackEnriched]]

  }
object GenresAlg {

  def create[F[_]: Monad: LoggerFactory](albumAlg: AlbumAlg[F], artistAlg: ArtistAlg[F]): GenresAlg[F] = new GenresAlg[F] {

    val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger
    
    def getSeveralAlbumGenres(sessionId: SessionId, albumIds: Seq[String])
                             (implicit raiseSessionError: RaiseSessionError[F]): F[Map[String, Seq[String]]] = {

      for {
        albumsOption <- albumAlg.getSeveralAlbums(sessionId, albumIds)
        albums = albumsOption.filter(_.isDefined).map(_.get)
        genres = albums.flatMap(album => Map(album.id -> album.genres)).toMap
      } yield genres

    }

    def getSeveralArtistGenres(sessionId: SessionId, artistIds: Seq[String])
                             (implicit raiseSessionError: RaiseSessionError[F]): F[Map[String, Seq[String]]] = {

      for {
        artistsOption: Seq[ArtistResponseType] <- artistAlg.getSeveralArtist(sessionId, artistIds)
        artists = artistsOption.filter(_.isDefined).map(_.get)
        genres: Map[String, Seq[String]] = artists.flatMap(artist => Map(artist.id -> artist.genres)).toMap
      } yield genres

    }

    def getSeveralTracksGenres(sessionId: SessionId, tracks: Seq[Track])
                              (implicit raiseSessionError: RaiseSessionError[F]): F[Seq[TrackEnriched]] = {

      for {
        _ <- logger.debug(s"Getting genres for several tracks")
        _ <- logger.debug(s"getSeveralTracksGenres, tracks(0): ${tracks.head}")
        artistsIds = tracks.flatMap(_.artists).map(_.id).distinct
        albumsIds = tracks.map(_.album.id).distinct
        _ <- logger.debug(s"getSeveralTracksGenres, albumsIds: $albumsIds")
        artistsGenres <- getSeveralArtistGenres(sessionId, artistsIds)
        albumsGenres <- getSeveralAlbumGenres(sessionId, albumsIds)
        savedTracksEnriched = tracks.map(
          track =>
            TrackEnriched.fromTrack(
              track,
              (track.artists.flatMap(artist=>artistsGenres(track.id)) ++ albumsGenres(track.album.id)).distinct
            ))
      } yield savedTracksEnriched
    }


    }


}
