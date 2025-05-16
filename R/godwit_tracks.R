#' Example Godwit Tracking Dataset
#'
#' A dataset of godwit tracking data used as an example for ezTrack functions.
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
#' @name godwit_tracks
#' @usage data(godwit_tracks)
#'
#' @examples
#' data(godwit_tracks)
#' godwit_tracks <- ez_track(godwit_tracks)
#' ez_summary(godwit_tracks)
#' ez_home_range(godwit_tracks)
"godwit_tracks"
