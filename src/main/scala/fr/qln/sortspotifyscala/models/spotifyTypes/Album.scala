package fr.qln.sortspotifyscala.models.spotifyTypes


case class Album(
                  album_type: String,
                  total_tracks: Int,
                  available_markets: List[String],
                  external_urls: ExternalUrls,
                  href: String,
                  id: String,
                  images: List[Image],
                  name: String,
                  release_date: String,
                  release_date_precision: String,
                  restrictions: Option[Restrictions],
                  `type`: String,
                  uri: String,
                  artists: List[SimplifiedArtist],
                  tracks: PaginatedResponse[Track],
                  copyrights: Seq[Copyrights],
                  external_ids: ExternalIds,
                  genres: Seq[String],
                  label: String,
                  popularity: String,
                ) 


case class AlbumResponse(albums: Seq[Album])