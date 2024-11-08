package fr.qln.sortspotifyscala.models.spotifyTypes

case class PaginatedResponse[ItemType](href: String,
                             limit: Int,
                             next: Option[String],
                             offset: Int,
                             previous: Option[String],
                             total: Int,
                             items: Seq[ItemType])
