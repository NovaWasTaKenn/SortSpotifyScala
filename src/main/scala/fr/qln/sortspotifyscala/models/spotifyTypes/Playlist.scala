package fr.qln.sortspotifyscala.models.spotifyTypes





// Case class for Playlist
case class Playlist(
                     collaborative: Boolean,
                     description: String,
                     external_urls: ExternalUrls,
                     followers: Followers,
                     href: String,
                     id: String,
                     images: List[Image],
                     name: String,
                     owner: Owner,
                     `public`: Boolean,
                     snapshot_id: String,
                     tracks: PaginatedResponse[TrackItem],
                     `type`: String,
                     uri: String
                   )

case class AddItemsResponse(snapshot_id: String)

case class CreatePlaylistBody(name: String, public: Boolean, collaborative: Boolean, description: String)

case class SortedTrack(name: String, trackIds: Seq[SavedTrackEnriched])