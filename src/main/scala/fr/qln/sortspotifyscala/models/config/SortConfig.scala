package fr.qln.sortspotifyscala.models.config

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

case class SortConfigJson(playlistConfigs: Seq[PlaylistConfigJson])
object SortConfigJson {
  implicit val sortConfigDecoder: Decoder[SortConfigJson] = deriveDecoder[SortConfigJson]
}

case class SortConfig(playlistConfigs: Seq[PlaylistConfig])
