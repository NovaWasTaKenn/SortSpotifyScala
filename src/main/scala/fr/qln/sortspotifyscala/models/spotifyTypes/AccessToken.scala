package fr.qln.sortspotifyscala.models.spotifyTypes

case class AccessToken(
                                access_token: String,
                                token_type: String,
                                expires_in: Int,
                                refresh_token: String,
                                scope: String
                              )
