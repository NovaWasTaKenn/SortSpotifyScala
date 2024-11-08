package fr.qln.sortspotifyscala.models.spotifyTypes


case class SavedTrackItem(added_at: String, track: Track)
case class SavedTrackEnriched(addedAt: String, trackWithGenres: TrackEnriched)
