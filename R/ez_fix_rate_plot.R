#' Plot Fix Rate Timelines for Each Individual
#'
#' Shows each individual's fix events as horizontal dot segments over time,
#' allowing quick assessment of fix density, coverage gaps, and tracking effort.
#'
#' @param data A data frame or `sf` object with columns `id` and `timestamp`.
#' @param date_breaks Optional. Interval between x-axis breaks (e.g., "1 month", "2 weeks"). Default is "1 month".
#' @param date_format Optional. Date label format (e.g., "\%b \%Y", "\%d-\%m"). Default is "\%d \%b \%Y".
#' @param start_date Optional. A `Date` or string (e.g., "2024-01-01"). Filters out fixes before this date.
#' @param end_date Optional. A `Date` or string (e.g., "2024-12-31"). Filters out fixes after this date.
#'
#' @return A ggplot object.
#'
#' @examples
#' df <- data.frame(
#'   id = rep(c("a", "b"), each = 3),
#'   timestamp = as.POSIXct(c("2025-01-01", "2025-01-02", "2025-01-03",
#'                            "2025-01-01", "2025-01-05", "2025-01-06"))
#' )
#' ez_fix_rate_plot(df)

#'
#' @export
ez_fix_rate_plot <- function(data,
                             date_breaks = "1 month",
                             date_format = "%d %b %Y",
                             start_date = NULL,
                             end_date = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install the 'ggplot2' package.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install the 'dplyr' package.")

  if (!all(c("id", "timestamp") %in% names(data))) {
    stop("Input must include columns: 'id' and 'timestamp'")
  }

  # Ensure timestamp is POSIXct
  data$timestamp <- as.POSIXct(data$timestamp)

  # Apply start_date filter
  if (!is.null(start_date)) {
    if (inherits(start_date, "character")) start_date <- as.Date(start_date)
    data <- data[data$timestamp >= as.POSIXct(start_date), ]
  }

  # Apply end_date filter
  if (!is.null(end_date)) {
    if (inherits(end_date, "character")) end_date <- as.Date(end_date)
    data <- data[data$timestamp <= as.POSIXct(end_date + 1) - 1, ]
  }


  data <- data |>
    dplyr::mutate(id = as.character(id)) |>
    dplyr::arrange(id, timestamp)

  # Order individuals by tracking duration (latest first)
  id_order <- data |>
    dplyr::group_by(id) |>
    dplyr::summarise(first = max(timestamp), last = min(timestamp), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(first)) |>
    dplyr::pull(id)

  data$y_labels <- factor(data$id, levels = id_order)

  ggplot2::ggplot(data, ggplot2::aes(x = timestamp, y = y_labels)) +
    ggplot2::geom_point(shape = 124, size = 3, color = "blue") +
    ggplot2::scale_x_datetime(date_breaks = date_breaks, date_labels = date_format) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

}
