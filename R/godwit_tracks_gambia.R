#' Example Godwit Tracking Dataset from Gambia
#'
#' A dataset of godwit tracking data from Gambia used as an example for ezTrack functions.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{individual.local.identifier}{Unique identifier for each tracked animal}
#'   \item{timestamp}{Datetime of the location fix}
#'   \item{location.long}{Longitude coordinate (WGS84)}
#'   \item{location.lat}{Latitude coordinate (WGS84)}
#' }
#' @source Movebank
#'
#' @docType data
#' @keywords datasets
#' @name godwit_tracks_gambia
#' @usage data(godwit_tracks_gambia)
#'
#' @examples
#' data(godwit_tracks_gambia)
#' godwit_tracks_gambia <- ez_track(godwit_tracks_gambia)
#' ez_summary(godwit_tracks_gambia)
#' ez_home_range(godwit_tracks_gambia)
"godwit_tracks_gambia"
