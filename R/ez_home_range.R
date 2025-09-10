#' Estimate Home Ranges for Tracked Individuals or Population
#'
#' Computes home range polygons using either Minimum Convex Polygon (MCP) or Kernel Density Estimation (KDE).
#'
#' @param data A data frame or `sf` object with columns `id`, `timestamp`, `x`, and `y`.
#' @param method Method for home range estimation. One of `"mcp"` (default) or `"kde"`.
#' @param level Percentage of points to include in the home range (e.g., 95 for 95\%). Default is 95.
#' @param crs Optional CRS to project the data before calculation. If NULL, uses EPSG:3857 (Web Mercator).
#' @param population Logical. If TRUE, returns a single home range polygon for all data combined.
#' @param start_date Optional. A `Date` object or string ("YYYY-MM-DD"). Filters out data before this date.
#' @param end_date Optional. A `Date` object or string ("YYYY-MM-DD"). Filters out data after this date.
#' @param kde_extent Numeric. When method = "kde", passed to `adehabitatHR::kernelUD()` to control the extent of the grid for KDE. Default is 1.
#' @param h Bandwidth method when method = "kde". One of `"href"` (default), "LSCV", or a numeric value.
#' @param hlim Optional vector of length 2 passed to `adehabitatHR::kernelUD()` when method = "kde" to constrain the bandwidth search (used with h = "LSCV").
#'
#' @return An `sf` object of home range polygon(s).
#' @export
ez_home_range <- function(data,
                          method = "mcp",
                          level = 95,
                          crs = NULL,
                          population = FALSE,
                          start_date = NULL,
                          end_date = NULL,
                          kde_extent = 1,
                          h = "href",
                          hlim = NULL) {

  # Load required packages
  if (!requireNamespace("sf", quietly = TRUE)) stop("Please install the 'sf' package.")
  if (!requireNamespace("sp", quietly = TRUE)) stop("Please install the 'sp' package.")
  if (!requireNamespace("adehabitatHR", quietly = TRUE)) stop("Please install the 'adehabitatHR' package.")

  # Convert to sf object if necessary
  if (!inherits(data, "sf")) {
    data <- sf::st_as_sf(data, coords = c("x", "y"), crs = 4326, remove = FALSE)
  }

  # Filter by date range if provided
  if (!is.null(start_date)) {
    if (inherits(start_date, "character")) start_date <- as.Date(start_date)
    data <- data[data$timestamp >= as.POSIXct(start_date), ]
  }

  if (!is.null(end_date)) {
    if (inherits(end_date, "character")) end_date <- as.Date(end_date)
    data <- data[data$timestamp <= as.POSIXct(end_date + 1) - 1, ]
  }

  if (nrow(data) == 0) {
    stop("No data remaining after applying start_date and end_date filters.")
  }

  # Reproject to specified or default CRS (Web Mercator)
  crs <- crs %||% 3857
  data_proj <- sf::st_transform(data, crs)

  # If population = TRUE, collapse all IDs into one
  if (population) {
    data_proj$id <- "population"
  }

  # Count number of points per ID
  id_counts <- table(data_proj$id)

  # Enforce method-specific minimum point thresholds
  min_points <- if (method == "kde") 10 else if (method == "mcp") 3 else Inf

  # Keep only individuals meeting the minimum requirement
  valid_ids <- names(id_counts[id_counts >= min_points])
  data_proj <- data_proj[data_proj$id %in% valid_ids, ]

  if (nrow(data_proj) == 0) {
    stop(paste0("No individuals with at least ", min_points, " points for method '", method, "'."))
  }

  # Convert to SpatialPointsDataFrame
  sp_points <- as(data_proj, "Spatial")
  sp_points@data <- data.frame(id = data_proj$id)

  # ---- MCP Method ----
  if (method == "mcp") {
    mcp_result <- adehabitatHR::mcp(sp_points[, "id"], percent = level)
    mcp_sf <- sf::st_as_sf(mcp_result)
    names(mcp_sf)[1] <- "id"
    return(sf::st_transform(mcp_sf, 4326))
  }

  # ---- KDE Method ----
  if (method == "kde") {
    # Assemble args for kernelUD
    kernel_args <- list(
      x = sp_points,
      h = h,
      same4all = population,
      extent = kde_extent
    )
    if (!is.null(hlim)) {
      kernel_args$hlim <- hlim
    }

    kde_ud <- do.call(adehabitatHR::kernelUD, kernel_args)
    kde_result <- adehabitatHR::getverticeshr(kde_ud, percent = level)
    kde_sf <- sf::st_as_sf(kde_result)
    names(kde_sf)[1] <- "id"
    return(sf::st_transform(kde_sf, 4326))
  }

  # Invalid method
  stop("Unsupported method: use 'mcp' or 'kde'")
}
