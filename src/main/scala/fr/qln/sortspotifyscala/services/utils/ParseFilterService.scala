package fr.qln.sortspotifyscala.services.utils

import cats.Monad
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.config.{AddedAt, Artists, Genres, SortFilterTarget}
import fr.qln.sortspotifyscala.models.errors.DomainErrors.{FilterTokenDoesntExist, SortError}
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.RaiseSortError
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}

import java.time.LocalDate
import scala.util.matching.Regex

trait ParseFilterService[F[_]] {
  def parseFilter(filter: String)(implicit raiseSortError: RaiseSortError[F]): F[Seq[SortFilterTarget] => Boolean]

}
object ParseFilterService{

  def create[F[_]: Monad: LoggerFactory](): ParseFilterService[F] = new ParseFilterService[F] {

    val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

    enum FilterTokens(val strRepresentation: Regex) {
      case GENRE extends FilterTokens("""^genre='([^&]+)'$""".r)
      case ARTIST extends FilterTokens("""^artist='([^&]+)'$""".r)
      case ADDED_AT extends FilterTokens("""^added_at([=><])'([^&]+)'$""".r)
      case GROUP extends FilterTokens("""^\((.+)\)$""".r)
      case OR extends FilterTokens("""^(\(?[^()]+\)?)\|(\(?[^()]+\)?)$""".r)
      case AND extends FilterTokens("""^(\(?[^()]+\)?)&(\(?[^()]+\)?)$""".r)
    }

    //private def getTarget[TargetType<: SortFilterTarget](targets: Seq[SortFilterTarget])(implicit raiseSortError: RaiseSortError[F]): F[TargetType]=
    //  for {
    //    _ <- logger.debug(s"Getting genres target")
    //    genresTargets = targets.filter(_.isInstanceOf[TargetType])
    //    genresTarget <- genresTargets.length match
    //      case 0 => FilterTargetsShouldHaveOneOfEachTargets.raise[F, TargetType]
    //      case 1 => genresTargets.head.asInstanceOf[TargetType].pure[F]
    //      case _ => FilterTargetsShouldHaveOneOfEachTargets.raise[F, TargetType]
    //  } yield genresTarget
      
      
    //private def genresFilter(targets: Seq[SortFilterTarget], genresFilter: String)(implicit raiseSortError: RaiseSortError[F]): F[Boolean] =
    //    for {
    //      genresTarget <- getTarget[Genres](targets)
    //      matchFound =  genresFilter.r.findFirstMatchIn(genresTarget.genres.mkString(",")).isDefined
    //    } yield matchFound
//
    //private def artistsFilter(targets: Seq[SortFilterTarget], artistsFilter: String)(implicit raiseSortError: RaiseSortError[F]): F[Boolean] =
    //  for {
    //    artistsTarget <- getTarget[Artists](targets)
    //    matchFound = artistsFilter.r.findFirstMatchIn(artistsTarget.artists.mkString(",")).isDefined
    //  } yield matchFound

    private def andFilters(left: F[Seq[SortFilterTarget] => Boolean], right: F[Seq[SortFilterTarget] => Boolean]): F[Seq[SortFilterTarget] => Boolean] = {
      for {
        leftFilter <- left
        rightFilter <- right
        combinedFilter = (targets: Seq[SortFilterTarget])=>leftFilter(targets) && rightFilter(targets)
      } yield combinedFilter
    }

    private def orFilters(left: F[Seq[SortFilterTarget] => Boolean], right: F[Seq[SortFilterTarget] => Boolean]): F[Seq[SortFilterTarget] => Boolean] = for {
      leftFilter <- left
      rightFilter <- right
      combinedFilter = (targets: Seq[SortFilterTarget])=>leftFilter(targets) || rightFilter(targets)
    } yield combinedFilter

    private def targetValidation(targets: Seq[SortFilterTarget], filter: Seq[SortFilterTarget] => Boolean):  Boolean =
        (targets.count(_.isInstanceOf[Genres]), targets.count(_.isInstanceOf[Artists]), targets.count(_.isInstanceOf[AddedAt])) match
          case (1,1,1) => filter(targets.filter(_.isInstanceOf[Genres])++targets.filter(_.isInstanceOf[Artists])++targets.filter(_.isInstanceOf[AddedAt]))
          case (_,_,_) => false


    private def parseFilterNoValidation(filter: String)(implicit raiseSortError: RaiseSortError[F]): F[Seq[SortFilterTarget] => Boolean] = {

      filter match
        case FilterTokens.GROUP.strRepresentation(group) =>
          parseFilter(group)
        case FilterTokens.OR.strRepresentation(left, right) =>
          orFilters(parseFilter(left)(raiseSortError), parseFilter(right)(raiseSortError))
        case FilterTokens.AND.strRepresentation(left, right) =>
          andFilters(parseFilter(left)(raiseSortError), parseFilter(right)(raiseSortError))
        case FilterTokens.GENRE.strRepresentation(genresFilter) =>
          ((targets: Seq[SortFilterTarget]) => genresFilter.r.findFirstMatchIn(targets.filter(_.isInstanceOf[Genres]).head.asInstanceOf[Genres].genres.mkString(",")).isDefined).pure[F]
        case FilterTokens.ARTIST.strRepresentation(artistsFilter) =>
          ((targets: Seq[SortFilterTarget]) => artistsFilter.r.findFirstMatchIn(targets.filter(_.isInstanceOf[Artists]).head.asInstanceOf[Artists].artists.mkString(",")).isDefined).pure[F]
        case FilterTokens.ADDED_AT.strRepresentation(operator, date) =>
          operator match
            case ">" => ((targets: Seq[SortFilterTarget]) => targets.filter(_.isInstanceOf[AddedAt]).head.asInstanceOf[AddedAt].date.isAfter(LocalDate.parse(date))).pure[F]
            case "<" => ((targets: Seq[SortFilterTarget]) => targets.filter(_.isInstanceOf[AddedAt]).head.asInstanceOf[AddedAt].date.isBefore(LocalDate.parse(date))).pure[F]
            case _ => raiseSortError.raise[SortError, Seq[SortFilterTarget] => Boolean](FilterTokenDoesntExist)
        case _ => raiseSortError.raise[SortError, Seq[SortFilterTarget] => Boolean](FilterTokenDoesntExist)

    }


    def parseFilter(filter: String)(implicit raiseSortError: RaiseSortError[F]): F[Seq[SortFilterTarget] => Boolean] =

      for {
        filter <- parseFilterNoValidation(filter)
        filterWithValidation = targetValidation(_, filter)
      } yield filterWithValidation
   }


}

