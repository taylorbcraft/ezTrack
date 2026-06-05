#' Summarize Animal Tracking Data
#'
#' Calculate basic summary statistics per tracked individual. This function is useful
#' for quickly understanding data coverage, gaps, and movement distance for each animal.
#' The following summary statistics are returned for each unique `id`:
#' - `n_fixes`: Number of location records
#' - `first_location`: Timestamp of the first recorded location
#' - `last_location`: Timestamp of the last recorded location
#' - `tracking_duration_days`: Duration between first and last fix (in days)
#' - `fixes_per_day`: Average number of fixes per day
#' - `median_interval_hours`: Median interval between fixes (in hours)
#' - `max_time_gap_days`: Longest time gap between consecutive fixes (in days)
#' - `distance_km`: Total distance traveled (in kilometers), calculated using the Haversine formula
#' - `avg_speed_kmh`: Average speed (km/h), computed as distance divided by tracking duration in hours
#'
#' @param data A data frame or sf object with columns `id`, `timestamp`, `x`, and `y`.
#' @param report Logical. If `TRUE`, writes an HTML summary table to a temporary file.
#' @param open_report Logical. If `TRUE` and `report = TRUE`, opens the HTML report in
#'   a browser. Defaults to `interactive()`.
#' @param start_date Optional. A `Date` object or string (e.g., "2021-01-01"). Filters out data before this date.
#' @param end_date Optional. A `Date` object or string (e.g., "2021-01-15"). Filters out data after this date.
#'
#' @return A data frame with summary statistics per `id`. When `report = TRUE`, the
#'   data frame is returned invisibly and includes a `report_path` attribute pointing
#'   to the generated HTML file.
#' @export
#'
#' @examples
#' data(godwit_tracks)
#' clean <- ez_track(godwit_tracks)
#' ez_summary(clean)

ez_summary <- function(data,
                       start_date = NULL,
                       end_date = NULL,
                       report = FALSE,
                       open_report = interactive()) {
  if (!requireNamespace("geosphere", quietly = TRUE)) {
    stop("The 'geosphere' package is required for ez_summary(). Please install it.")
  }

  if (report && !requireNamespace("knitr", quietly = TRUE)) {
    stop("The 'knitr' package is required to return HTML tables. Please install it or set report = FALSE.")
  }

  if (!all(c("id", "x", "y", "timestamp") %in% names(data))) {
    stop("Data must contain columns: id, timestamp, x, y")
  }

  data <- data[order(data$id, data$timestamp), ]

  if (!is.null(start_date)) {
    if (inherits(start_date, "character")) start_date <- as.Date(start_date)
    data <- data[data$timestamp >= as.POSIXct(start_date), ]
  }

  if (!is.null(end_date)) {
    if (inherits(end_date, "character")) end_date <- as.Date(end_date)
    data <- data[data$timestamp <= as.POSIXct(end_date + 1) - 1, ]
  }

  summary_list <- lapply(split(data, data$id), function(track) {
    n <- nrow(track)
    first_location <- min(track$timestamp, na.rm = TRUE)
    last_location <- max(track$timestamp, na.rm = TRUE)
    duration_days <- as.numeric(difftime(last_location, first_location, units = "days"))
    duration_hours <- as.numeric(difftime(last_location, first_location, units = "hours"))

    if (n >= 2) {
      time_diffs <- as.numeric(diff(track$timestamp), units = "days")
      median_interval <- round(median(time_diffs * 24, na.rm = TRUE), 2)
      max_gap <- round(max(time_diffs, na.rm = TRUE), 2)
      coords <- cbind(track$x, track$y)
      dists <- geosphere::distHaversine(coords[-n, ], coords[-1, ]) / 1000
      total_dist <- sum(dists, na.rm = TRUE)
      avg_speed <- total_dist / duration_hours
    } else {
      median_interval <- NA
      max_gap <- NA
      total_dist <- 0
      avg_speed <- NA
    }

    data.frame(
      id = track$id[1],
      n_fixes = n,
      first_location = first_location,
      last_location = last_location,
      tracking_duration_days = round(duration_days, 2),
      fixes_per_day = round(n / max(duration_days, 1), 2),
      median_interval_hours = median_interval,
      max_time_gap_days = max_gap,
      distance_km = round(total_dist, 2),
      avg_speed_kmh = round(avg_speed, 2),
      stringsAsFactors = FALSE
    )
  })

  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- NULL

  if (report) {
    html_table <- knitr::kable(summary_df, format = "html", digits = 2, escape = FALSE)

    html_body <- paste0(
      "<html><head><meta charset='utf-8'>",
      "<title>ezTrack Summary Report</title>",
      "<style>",
      "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;margin:2rem;line-height:1.4;}",
      "h2{margin-bottom:1rem;}",
      "table{border-collapse:collapse;width:100%;max-width:960px;}",
      "th,td{border:1px solid #d9d9d9;padding:0.5rem;text-align:left;vertical-align:top;}",
      "thead th{background:#f5f5f5;}",
      "tbody tr:nth-child(even){background:#fafafa;}",
      "</style>",
      "</head><body><h2>ezTrack Summary Report</h2>",
      as.character(html_table),
      "</body></html>"
    )

    report_path <- tempfile(fileext = ".html")
    writeLines(html_body, con = report_path, useBytes = TRUE)
    attr(summary_df, "report_path") <- report_path

    if (isTRUE(open_report) && interactive()) {
      utils::browseURL(report_path)
    }

    return(invisible(summary_df))
  }

  return(summary_df)
}
