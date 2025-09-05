#' Estimate Home Ranges for Tracked Individuals or Population
#'
#' Computes home range polygons using either Minimum Convex Polygon (MCP) or Kernel Density Estimation (KDE).
#'
#' @param data A data frame or `sf` object with columns `id`, `timestamp`, `x`, and `y`.
#' @param method Method for home range estimation. One of "mcp" (default) or "kde".
#' @param level Percentage of points to include in the home range (e.g., 95 for 95\%). Default is 95.
#' @param crs Optional CRS to project the data before calculation.
#'   If `NULL`, the function will automatically detect the UTM zone of the dataset centroid
#'   and reproject data accordingly (units in meters).
#' @param population Logical. If TRUE, returns a single home range polygon for all data combined.
#' @param start_date Optional. A `Date` object or string ("YYYY-MM-DD"). Filters out data before this date.
#' @param end_date Optional. A `Date` object or string ("YYYY-MM-DD"). Filters out data after this date.
#' @param kde_extent Numeric. When method = "kde", passed to `adehabitatHR::kernelUD()` to control the extent of the grid for KDE. Default is 1.
#' @param h Bandwidth method when method = "kde". One of "href" (default), "LSCV", or a numeric value.
#' @param hlim Optional vector of length 2 passed to `adehabitatHR::kernelUD()` when method = "kde" to constrain the bandwidth search (used with h = "LSCV").
#'
#' @return An `sf` object of utilization distribution polygon(s).
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

  if (!requireNamespace("sf", quietly = TRUE)) stop("Please install the 'sf' package.")
  if (!requireNamespace("sp", quietly = TRUE)) stop("Please install the 'sp' package.")
  if (!requireNamespace("adehabitatHR", quietly = TRUE)) stop("Please install the 'adehabitatHR' package.")

  # Convert to sf object if necessary
  if (!inherits(data, "sf")) {
    data <- sf::st_as_sf(data, coords = c("x", "y"), crs = 4326, remove = FALSE)
  }

  # Filter by date range
  if (!is.null(start_date)) {
    if (inherits(start_date, "character")) start_date <- as.Date(start_date)
    data <- data[data$timestamp >= as.POSIXct(start_date), ]
  }
  if (!is.null(end_date)) {
    if (inherits(end_date, "character")) end_date <- as.Date(end_date)
    data <- data[data$timestamp <= as.POSIXct(end_date + 1) - 1, ]
  }
  if (nrow(data) == 0) stop("No data remaining after applying start_date and end_date filters.")

  # Default CRS: choose UTM zone based on centroid
  if (is.null(crs)) {
    centroid <- sf::st_coordinates(sf::st_centroid(sf::st_union(data)))
    utm_zone <- floor((centroid[1] + 180) / 6) + 1
    crs <- paste0("EPSG:", ifelse(centroid[2] >= 0, 32600, 32700) + utm_zone)
  }

  data_proj <- sf::st_transform(data, crs)

  # Collapse to one ID if population = TRUE
  if (population) {
    data_proj$id <- "population"
  }

  id_counts <- table(data_proj$id)
  min_points <- if (method == "kde") 10 else if (method == "mcp") 3 else Inf
  valid_ids <- names(id_counts[id_counts >= min_points])
  data_proj <- data_proj[data_proj$id %in% valid_ids, ]
  if (nrow(data_proj) == 0) {
    stop(paste0("No individuals with at least ", min_points, " points for method '", method, "'."))
  }

  # Convert to SpatialPointsDataFrame
  sp_points <- as(data_proj, "Spatial")
  sp_points@data <- data.frame(id = data_proj$id)

  # ---- MCP ----
  if (method == "mcp") {
    mcp_result <- adehabitatHR::mcp(sp_points[, "id"], percent = level)
    mcp_sf <- sf::st_as_sf(mcp_result)
    names(mcp_sf)[1] <- "id"
    return(sf::st_transform(mcp_sf, 4326))
  }

  # ---- KDE ----
  if (method == "kde") {
    kernel_args <- list(x = sp_points, h = h, same4all = population, extent = kde_extent)
    if (!is.null(hlim)) kernel_args$hlim <- hlim
    kde_ud <- do.call(adehabitatHR::kernelUD, kernel_args)
    kde_result <- adehabitatHR::getverticeshr(kde_ud, percent = level)
    kde_sf <- sf::st_as_sf(kde_result)
    names(kde_sf)[1] <- "id"
    return(sf::st_transform(kde_sf, 4326))
  }

  stop("Unsupported method: use 'mcp' or 'kde'")
}
