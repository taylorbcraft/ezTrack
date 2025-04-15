#' Summarize Animal Tracking Data
#'
#' Calculate basic summary statistics per tracked individual.
#'
#' @param data A data frame or sf object with columns id, timestamp, x, and y.
#'
#' @return A data frame with summary statistics per `id`.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- import_tracking_data("...")
#' summarize_tracks(df)
#' }
ez_summary <- function(data) {
  if (!requireNamespace("geosphere", quietly = TRUE)) {
    stop("The 'geosphere' package is required for summarize_tracks(). Please install it.")
  }

  if (!all(c("id", "x", "y", "timestamp") %in% names(data))) {
    stop("Data must contain columns: id, timestamp, x, y")
  }

  data <- data[order(data$id, data$timestamp), ]

  summary_list <- lapply(split(data, data$id), function(track) {
    n <- nrow(track)
    first_location <- min(track$timestamp, na.rm = TRUE)
    last_location <- max(track$timestamp, na.rm = TRUE)
    tracking_duration_days <- as.numeric(difftime(last_location, first_location, units = "days"))
    duration_hours <- as.numeric(difftime(last_location, first_location, units = "hours"))

    # Time intervals
    time_diffs <- as.numeric(diff(track$timestamp), units = "days")
    median_interval <- round(median(time_diffs * 24, na.rm = TRUE), 2)  # minutes
    max_gap_days <- round(max(time_diffs, na.rm = TRUE), 2)

    # Total distance
    coords <- cbind(track$x, track$y)
    dists <- geosphere::distHaversine(coords[-n, ], coords[-1, ]) / 1000  # km
    total_dist <- sum(dists, na.rm = TRUE)

    data.frame(
      id = track$id[1],
      n_fixes = n,
      first_location = first_location,
      last_location = last_location,
      tracking_duration_days = round(tracking_duration_days, 2),
      fixes_per_day = round(n / tracking_duration_days, 2),
      median_interval_hours = median_interval,
      max_time_gap_days = max_gap_days,
      distance_km = round(total_dist, 2),
      avg_speed_kmh = round(total_dist / duration_hours, 2)
    )
  })


  do.call(rbind, summary_list)
}
