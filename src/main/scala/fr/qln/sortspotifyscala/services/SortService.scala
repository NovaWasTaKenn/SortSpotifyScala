package fr.qln.sortspotifyscala.services

import cats.Monad
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.config.{AddedAt, Artists, Genres, SortConfig}
import fr.qln.sortspotifyscala.models.spotifyTypes.{SavedTrackEnriched, SortedTrack}
import fr.qln.sortspotifyscala.services.spotify.UserAlg
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}

import java.time.LocalDate


trait SortService[F[_]] {
  def sortByGenre(tracks: Seq[SavedTrackEnriched], sortConfig: SortConfig): F[Seq[SortedTrack]]
}
object SortService {

  def create[F[_]: Monad: LoggerFactory](userAlg: UserAlg[F]): SortService[F] = new SortService[F] {
    
    val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger
    
    
    def sortByGenre(tracks: Seq[SavedTrackEnriched], sortConfig: SortConfig): F[Seq[SortedTrack]] =

      sortConfig.playlistConfigs.map(
        playlistConfig =>
          SortedTrack(
            playlistConfig.playlistName,
            tracks.filter(
              savedTrack =>
                playlistConfig.filter(Seq(Artists(savedTrack.trackWithGenres.artists.map(_.id)), Genres(savedTrack.trackWithGenres.genres), AddedAt(LocalDate.parse(savedTrack.addedAt))))
            )
          )
      ).pure[F]
  }

}