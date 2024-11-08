package fr.qln.sortspotifyscala.models.spotifyTypes

// Case class for Track
case class Track(
                  album: SimplifiedAlbum,
                  artists: List[SimplifiedArtist],
                  available_markets: List[String],
                  disc_number: Int,
                  duration_ms: Int,
                  explicit: Boolean,
                  external_ids: ExternalIds,
                  external_urls: ExternalUrls,
                  href: String,
                  id: String,
                  is_playable: Boolean,
                  restrictions: Option[Restrictions],
                  name: String,
                  popularity: Int,
                  preview_url: Option[String],
                  track_number: Int,
                  `type`: String,
                  uri: String,
                  is_local: Boolean
                )


case class TrackEnriched(
                          album: SimplifiedAlbum,
                          artists: List[SimplifiedArtist],
                          explicit: Boolean,
                          id: String,
                          name: String,
                          popularity: Int,
                          `type`: String,
                          uri: String,
                          genres: Seq[String]
                        )
object TrackEnriched {
  def fromTrack(track: Track, genres: Seq[String]): TrackEnriched=
    TrackEnriched(
      track.album, 
      track.artists, 
      track.explicit, 
      track.id, 
      track.name, 
      track.popularity, 
      track.`type`, 
      track.uri,
      genres
    )
}