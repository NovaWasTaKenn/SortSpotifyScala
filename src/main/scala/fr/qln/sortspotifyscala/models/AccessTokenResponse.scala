package fr.qln.sortspotifyscala.models


case class AccessTokenResponse(
                                access_token: String,
                                token_type: String,
                                expires_in: Int,
                                refresh_token: String,
                                scope: String
                              )
