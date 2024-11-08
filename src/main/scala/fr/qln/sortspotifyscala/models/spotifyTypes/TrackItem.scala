package fr.qln.sortspotifyscala.models.spotifyTypes

// Case class for Track Item
case class TrackItem(
                      added_at: String,
                      added_by: AddedBy,
                      is_local: Boolean,
                      track: Track
                    )

