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
#' @param report Logical. If TRUE, opens an HTML summary table in your browser for easy copying into slides or documents.
#' @param startDate Optional. A `Date` object or string (e.g., "2021-01-01"). Filters out data before this date.
#' @param endDate Optional. A `Date` object or string (e.g., "2021-01-15"). Filters out data after this date.
#'
#' @return A data frame with summary statistics per `id`, or an HTML table if `report = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ez_track("tracking_data.csv")
#' ez_summary(df)
#' ez_summary(df, report = TRUE)
#' }
ez_summary <- function(data, startDate = NULL, endDate = NULL, report = FALSE) {
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

  # Convert date strings to Date objects if needed
  if (!is.null(startDate)) {
    if (inherits(startDate, "character")) startDate <- as.Date(startDate)
    if (!inherits(startDate, "Date")) stop("startDate must be a Date or a string in 'YYYY-MM-DD' format.")
    data <- data[data$timestamp >= as.POSIXct(startDate), ]
  }

  if (!is.null(endDate)) {
    if (inherits(endDate, "character")) endDate <- as.Date(endDate)
    if (!inherits(endDate, "Date")) stop("endDate must be a Date or a string in 'YYYY-MM-DD' format.")
    data <- data[data$timestamp <= as.POSIXct(endDate + 1) - 1, ]
  }

  summary_list <- lapply(split(data, data$id), function(track) {
    n <- nrow(track)
    first_location <- min(track$timestamp, na.rm = TRUE)
    last_location <- max(track$timestamp, na.rm = TRUE)
    tracking_duration_days <- as.numeric(difftime(last_location, first_location, units = "days"))
    duration_hours <- as.numeric(difftime(last_location, first_location, units = "hours"))

    # Time intervals
    time_diffs <- as.numeric(diff(track$timestamp), units = "days")
    median_interval <- round(median(time_diffs * 24, na.rm = TRUE), 2)  # hours
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
      avg_speed_kmh = round(total_dist / duration_hours, 2),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  summary_df <- do.call(rbind, summary_list)
  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- NULL


  if (report) {
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
      stop("The 'kableExtra' package is required for HTML table reports.")
    }
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      stop("The 'htmltools' package is required to save and open the HTML report.")
    }

    # Create the HTML table
    html_table <- knitr::kable(summary_df, format = "html", digits = 2, escape = FALSE) |>
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

    # Wrap in full HTML page using htmltools::HTML()
    html_body <- paste0(
      "<html><head>",
      "<title>ezTrack Summary Report</title>",
      "<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/bootstrap@3.3.7/dist/css/bootstrap.min.css'>",
      "</head><body>",
      "<div class='container'>",
      "<h2>ezTrack Summary Report</h2>",
      as.character(html_table),
      "</div></body></html>"
    )

    # Save and open the report
    tmp_file <- tempfile(fileext = ".html")
    writeLines(html_body, con = tmp_file)
    utils::browseURL(tmp_file)

    return(invisible(summary_df))
  }



  return(summary_df)
}
