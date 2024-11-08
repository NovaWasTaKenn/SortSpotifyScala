package fr.qln.sortspotifyscala.models.config

import java.time.LocalDate

trait SortFilterTarget 
case class Genres(genres: Seq[String]) extends SortFilterTarget 
case class Artists(artists: Seq[String]) extends SortFilterTarget
case class AddedAt(date: LocalDate) extends SortFilterTarget