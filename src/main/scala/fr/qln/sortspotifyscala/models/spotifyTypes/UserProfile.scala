package fr.qln.sortspotifyscala.models.spotifyTypes


case class UserProfile(
                                country: String,
                                display_name: String,
                                email: String,
                                explicit_content:  ExplicitContent,
                                external_urls: ExternalUrls,
                                followers: Followers,
                                href: String,
                                id: String,
                                images: Seq[Image] = Seq.empty[Image],
                                product: String,
                                uri: String,
                                `type`: String
                              )
