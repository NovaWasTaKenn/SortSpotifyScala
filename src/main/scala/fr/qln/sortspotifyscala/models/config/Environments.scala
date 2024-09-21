package fr.qln.sortspotifyscala.models.config

import enumeratum.{Enum, CirisEnum, EnumEntry}
import enumeratum.EnumEntry.Lowercase

object Environments {
  sealed trait AppEnvironment extends EnumEntry with Lowercase

  object AppEnvironment extends Enum[AppEnvironment] with CirisEnum[AppEnvironment] {
    case object Local extends AppEnvironment
    case object Testing extends AppEnvironment
    case object Production extends AppEnvironment

    val values: IndexedSeq[AppEnvironment] = findValues
  }
}
