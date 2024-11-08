package fr.qln.sortspotifyscala.models.spotifyTypes

// Case class for Owner
case class Owner(
                  external_urls: ExternalUrls,
                  followers: Followers,
                  href: String,
                  id: String,
                  `type`: String,
                  uri: String,
                  display_name: String
                )

