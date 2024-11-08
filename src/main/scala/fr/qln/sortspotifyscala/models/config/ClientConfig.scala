package fr.qln.sortspotifyscala.models.config

import scala.concurrent.duration.Duration

case class ClientConfig(retryMaxWait: Duration, retryMaxRetries: Int)
