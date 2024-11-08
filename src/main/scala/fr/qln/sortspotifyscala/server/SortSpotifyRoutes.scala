package fr.qln.sortspotifyscala
package server

import cats.effect.Concurrent
import cats.mtl.implicits.*
import cats.syntax.all.*
import fr.qln.sortspotifyscala.models.config.{ServerConfig, SortConfig}
import fr.qln.sortspotifyscala.models.errors.DomainErrors.*
import fr.qln.sortspotifyscala.models.spotifyTypes.*
import fr.qln.sortspotifyscala.models.types.ErrorHandlingTypes.*
import fr.qln.sortspotifyscala.services.SortService
import fr.qln.sortspotifyscala.services.auth.{AuthService, AuthValues, SessionId, SessionService}
import fr.qln.sortspotifyscala.services.spotify.{GenresAlg, PlaylistAlg, UserAlg}
import fr.qln.sortspotifyscala.services.utils.ConfigService
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.headers.Location
import org.http4s.{Header, HttpRoutes, Request, Response, ResponseCookie, Uri}
import org.typelevel.ci.CIString
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}


object SortSpotifyRoutes {

  private object StateQueryParamMatcher extends QueryParamDecoderMatcher[String]("state")

  private object CodeQueryParamMatcher extends QueryParamDecoderMatcher[String]("code")

  private object ErrorQueryParamMatcher extends QueryParamDecoderMatcher[String]("error")


  def loginRoutes[F[_] : Concurrent : LoggerFactory : HandleAuthError : ApplicativeAuthServiceError : HandleSessionError](authService: AuthService[F], sessionService: SessionService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

    HttpRoutes.of[F] {
      case GET -> Root / "login" =>

        (for {
          _ <- logger.info("login")
          newSession <- sessionService.newSession(authed = false, tokens = None, spotifyUserId = None)
          authValues: AuthValues <- authService.initAuthValues(newSession.sessionId)
          authorizeUrl = authService.createRedirectUrl(authValues.codeVerifier, authValues.state)
          response <- PermanentRedirect(Location(Uri.unsafeFromString(authorizeUrl)))
          finalResponse = response
            .addCookie(ResponseCookie("sessionId", newSession.sessionId.toString, path = Some("/"), maxAge = Some(3600), httpOnly = true, secure = false, sameSite = None)) //Add other params
            .putHeaders(Header.Raw(CIString("Cache-Control"), "max-age=0"))
        } yield finalResponse
          ).handleWith[SessionError]({
            case error@SessionAlreadyExists =>
              logger.error(error)("Session already exists with provided sessionId") >> BadRequest("Session already exists with provided sessionId")
            case error: SessionError =>
              logger.error(error)(s"Unexpected domain error: ${error.getMessage}") >> BadRequest(s"Unexpected domain error: ${error.getMessage}")
          })
          .handleWith[AuthenticationError]({
            case error@AuthenticationValuesAlreadyExists =>
              logger.error(error)("Authentication values already exists with provided authId") >> BadRequest("Authentication values already exists with provided authId")
            case error: AuthenticationError =>
              logger.error(error)(s"Unexpected domain error: ${error.getMessage}") >> BadRequest(s"Unexpected domain error: ${error.getMessage}")
          })
          .recoverWith({
            case error: Throwable => logger.error(error)("Internal server error") >> InternalServerError(error.getMessage)
          })
    }
  }

  def homeRoutes[F[_]: Concurrent](): HttpRoutes[F] =

    val dsl = new Http4sDsl[F] {}

    import dsl.*

    HttpRoutes.of[F] {

      case GET -> Root / "home" => Ok("Home page")

    }

  def authCallbackRoutes[F[_] : LoggerFactory : Concurrent : HandleAuthError : ApplicativeAuthServiceError]
  (config: ServerConfig, authService: AuthService[F], sessionService: SessionService[F], userProfileAlg: UserAlg[F])(implicit raiseSessionError: RaiseSessionError[F]): HttpRoutes[F] = {

    val dsl = new Http4sDsl[F] {}
    import dsl.*

    val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

    HttpRoutes.of[F] {

      case request@GET -> Root / "callback" :? CodeQueryParamMatcher(code) +& StateQueryParamMatcher(returnedState) =>


        (for {
          sessionId <- sessionService.getSessionId(request.cookies)
          
          tokens <- authService.getInitialTokens(authId = sessionId, code = code, returnedState = returnedState)
          
          newSession <- sessionService.newSession(true, Some(tokens), None)
          _ <- sessionService.removeSession(sessionId)
          
          spotifyUserProfile <- userProfileAlg.getSpotifyProfile(newSession.sessionId)
          _ <- sessionService.modifySession(newSession.copy(spotifyUserId = Some(spotifyUserProfile.id)))
          
          response <- PermanentRedirect(Location(Uri.unsafeFromString(config.authConfig.homeUrl)))
          finalResponse = response
            .addCookie(ResponseCookie("sessionId", newSession.sessionId.toString, path = Some("/"), maxAge = Some(3600), httpOnly = true, secure = false, sameSite = None)) //Add other params
            .putHeaders(Header.Raw(CIString("Cache-Control"), "max-age=0"))
        
        } yield finalResponse)
          .handleWith[AuthenticationError]({
            case error@AuthenticationValuesNotFound =>
              logger.error(error)("No authentication values found for provided authId") >> BadRequest("No authentication values found for provided authId")
            case error@AuthenticationValuesAlreadyExists =>
              logger.error(error)("Authentication values already exists with provided authId") >> BadRequest("Authentication values already exists with provided authId")
            case error@InvalidReturnedState =>
              logger.error(error)("Returned state different to stored state") >> BadRequest("Returned state different to stored state")
            case error: SessionError =>
              logger.error(error)(s"Unexpected domain error: ${error.getMessage}") >> BadRequest(s"Unexpected domain error: ${error.getMessage}")
          })
          .recoverWith({
            case error: Throwable => logger.error(error)("Internal server error") >> InternalServerError(error.getMessage)
          })


      case GET -> Root / "callback" :? ErrorQueryParamMatcher(error) +& StateQueryParamMatcher(state) => BadRequest(error)

    }
  }


  def sortRoutes[F[_]: LoggerFactory: Concurrent]
  (sessionService: SessionService[F], genresAlg: GenresAlg[F], configService: ConfigService[F], sortService: SortService[F], userAlg: UserAlg[F], playlistAlg: PlaylistAlg[F])
  (implicit handleSortError: HandleSortError[F], handleSessionError: HandleSessionError[F]): HttpRoutes[F] =

    val dsl = new Http4sDsl[F] {}
    import dsl.*

    val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

    HttpRoutes.of[F] {

      case request@POST -> Root / "sort"/ "genres" =>
        (for {
          sessionId: SessionId <- sessionService.getSessionId(request.cookies)
          sortConfig: SortConfig <- configService.getSortConfig(request)
          savedTracks: Seq[SavedTrackItem] <- userAlg.getSavedTracks(sessionId)
          savedTracksDistinct: Seq[SavedTrackItem] = savedTracks.distinctBy(_.track.id)
          tracksEnriched: Seq[TrackEnriched] <- genresAlg.getSeveralTracksGenres(sessionId, savedTracksDistinct.map(_.track))
          sortedTracksList: Seq[SortedTrack] <- sortService.sortByGenre(
            tracksEnriched.map(
              te=>SavedTrackEnriched(
                savedTracksDistinct.filter(_.track.id==te.id).head.added_at,
                te
              )
            ),
            sortConfig)
          createdPlaylists: Seq[Playlist] <- playlistAlg.createSeveralPlaylists(sessionId, sortConfig.playlistConfigs.map(pConf=> CreatePlaylistBody(pConf.playlistName, true, false, "")))
          snapShotIds: Seq[Any] = sortedTracksList.map(
              sortedTracks => playlistAlg.addItems(
                createdPlaylists.filter(_.name==sortedTracks.name).head.id,
                savedTracksDistinct.filter(st=>sortedTracks.trackIds.contains(st.track.id)).map(_.track.uri),
                sessionId
              )

          )
          response: Response[F] <- Ok("Musics sorted")
        } yield response)
          .handleWith[SessionError]({
            case error@TokensNotDefined=>
              logger.error(error)("tokens not found in session") >> BadRequest("tokens not found in session")
            case error@SessionNotAuthenticated =>
              logger.error(error)("Session not authenticated") >> BadRequest("Authentication values already exists with provided authId")
            case error: SessionError =>
              logger.error(error)(s"Unexpected domain error: ${error.getMessage}") >> BadRequest(s"Unexpected domain error: ${error.getMessage}")
          })
          .handleWith[SortError]({
            case error@FilterTargetsShouldHaveOneOfEachTargets =>
              logger.error(error)("FilterTargetsShouldHaveOneOfEachTargets") >> BadRequest("FilterTargetsShouldHaveOneOfEachTargets")
            case error@FilterTokenDoesntExist =>
              logger.error(error)("FilterTokenDoesntExist") >> BadRequest("FilterTokenDoesntExist")
            case error@SortConfigBodyHasWrongFormat =>
              logger.error(error)("SortConfigBodyHasWrongFormat") >> BadRequest("SortConfigBodyHasWrongFormat")
            case error: SortError =>
              logger.error(error)(s"Unexpected domain error: ${error.getMessage}") >> BadRequest(s"Unexpected domain error: ${error.getMessage}")
          })
          .recoverWith({
            case error: Throwable => logger.error(error)("Internal server error") >> InternalServerError(error.getMessage)
          })

    }


  def helloWorldRoutes[F[_] : Concurrent](): HttpRoutes[F] =
    ???

}
