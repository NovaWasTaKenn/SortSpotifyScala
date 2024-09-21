package fr.qln.sortspotifyscala.models.config

import ciris.ConfigDecoder
import ciris.circe.yaml.circeYamlConfigDecoder
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class AuthConfig(
                       clientId: String, 
                       scopes: String, 
                       callbackUrl: String, 
                       authUrl: String, 
                       tokenUrl: String, 
                       homeUrl: String)
object AuthConfig :
    given AuthConfigDecoder: Decoder[AuthConfig] = deriveDecoder[AuthConfig]
  
    given AuthConfigYamlDecoder: ConfigDecoder[String, AuthConfig] = circeYamlConfigDecoder[AuthConfig]("AuthConfig")