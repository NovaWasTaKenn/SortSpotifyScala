package fr.qln.sortspotifyscala.models.config

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

case class PlaylistConfigJson(playlistName: String, filter: String)
object PlaylistConfigJson {
  implicit val playlistConfigDecoder: Decoder[PlaylistConfigJson] = deriveDecoder[PlaylistConfigJson]
}

case class PlaylistConfig(playlistName: String, filter: Seq[SortFilterTarget]=>Boolean)

