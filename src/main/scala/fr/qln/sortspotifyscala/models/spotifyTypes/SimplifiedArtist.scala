package fr.qln.sortspotifyscala.models.spotifyTypes

// Case class for Artist
case class SimplifiedArtist(
                   external_urls: ExternalUrls,
                   href: String,
                   id: String,
                   name: String,
                   `type`: String,
                   uri: String
                 )
