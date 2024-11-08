package fr.qln.sortspotifyscala.models.spotifyTypes

case class Artist(external_urls: ExternalUrls,
                  followers: Followers,
                  genres: Seq[String],
                  href: String,
                  id: String,
                  images: Seq[Image],
                  name: String,
                  popularity: Int,
                  `type`: String,
                  uri: String
                 )
