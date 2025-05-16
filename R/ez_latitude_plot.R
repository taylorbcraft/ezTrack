#' Plot Latitude Over Time
#'
#' Creates a time series plot of latitude (y-axis) over timestamp (x-axis), with separate lines for each individual animal. Optionally facets the plot by animal and allows customization of x-axis date format and break spacing.
#'
#' @param data A data frame or `sf` object with columns `id`, `timestamp`, and `y` (latitude).
#' @param color_palette Character. Viridis palette option: "viridis", "magma", "plasma", "inferno", "cividis", or "turbo". Default is "turbo".
#' @param facet Logical. If TRUE, creates a separate facet panel for each animal. Default is FALSE.
#' @param date_format Optional. Format for date labels on the x-axis (e.g., "\%b \%d", "\%Y-\%m", "\%H:\%M"). Default is automatic.
#' @param date_breaks Optional. Interval for x-axis breaks (e.g., "1 day", "2 weeks"). Default is automatic.
#' @param startDate Optional. A `Date` object or string (e.g., "2023-01-01"). Filters out data before this date.
#' @param endDate Optional. A `Date` object or string (e.g., "2023-02-01"). Filters out data after this date.
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap scale_color_viridis_d scale_x_datetime labs theme_minimal theme

ez_latitude_plot <- function(data,
                             color_palette = "turbo",
                             facet = FALSE,
                             date_format = NULL,
                             date_breaks = NULL,
                             startDate = NULL,
                             endDate = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install the 'ggplot2' package.")
  if (!requireNamespace("viridisLite", quietly = TRUE)) stop("Please install the 'viridisLite' package.")

  if (!all(c("id", "timestamp", "y") %in% names(data))) {
    stop("Data must contain 'id', 'timestamp', and 'y' (latitude) columns.")
  }

  data$timestamp <- as.POSIXct(data$timestamp)

  # Filter by startDate
  if (!is.null(startDate)) {
    if (inherits(startDate, "character")) startDate <- as.Date(startDate)
    data <- data[data$timestamp >= as.POSIXct(startDate), ]
  }

  # Filter by endDate
  if (!is.null(endDate)) {
    if (inherits(endDate, "character")) endDate <- as.Date(endDate)
    data <- data[data$timestamp <= as.POSIXct(endDate + 1) - 1, ]
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = timestamp, y = y, color = id)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::scale_color_viridis_d(option = color_palette) +
    ggplot2::labs(
      x = NULL,
      y = "Latitude",
      color = "Animal ID"
    ) +
    ggplot2::theme_minimal()

  if (!is.null(date_format) || !is.null(date_breaks)) {
    scale_args <- list()
    if (!is.null(date_format)) scale_args$date_labels <- date_format
    if (!is.null(date_breaks)) scale_args$date_breaks <- date_breaks
    p <- p + do.call(ggplot2::scale_x_datetime, scale_args)
  }

  if (facet) {
    p <- p +
      ggplot2::facet_wrap(~ id, scales = "free_y") +
      ggplot2::theme(legend.position = "none")
  }

  return(p)
}
