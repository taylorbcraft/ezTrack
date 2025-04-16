#' Estimate Home Ranges for Tracked Individuals or Population
#'
#' Computes home range polygons using either Minimum Convex Polygon (MCP) or Kernel Density Estimation (KDE).
#'
#' @param data A data frame or `sf` object with columns `id`, `x`, and `y`.
#' @param method Method for home range estimation. One of `"mcp"` (default) or `"kde"`.
#' @param level Percentage of points to include in the home range (e.g., 95 for 95\%). Default is 95.
#' @param crs Optional CRS to project the data before calculation. If NULL, uses EPSG:3857 (Web Mercator).
#' @param population Logical. If TRUE, returns a single home range polygon for all data combined.
#'
#' @return An `sf` object of home range polygon(s).
#' @export
ez_home_range <- function(data, method = "mcp", level = 95, crs = NULL, population = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Please install the 'sf' package.")
  if (!requireNamespace("sp", quietly = TRUE)) stop("Please install the 'sp' package.")
  if (!requireNamespace("adehabitatHR", quietly = TRUE)) stop("Please install the 'adehabitatHR' package.")

  # Ensure sf object
  if (!inherits(data, "sf")) {
    data <- sf::st_as_sf(data, coords = c("x", "y"), crs = 4326, remove = FALSE)
  }

  crs <- crs %||% 3857
  data_proj <- sf::st_transform(data, crs)

  if (population) {
    data_proj$id <- "population"
  }

  # Convert to SpatialPointsDataFrame with ID only
  sp_points <- as(data_proj, "Spatial")
  sp_points@data <- data.frame(id = data_proj$id)  # Only the ID column

  if (method == "mcp") {
    mcp_result <- adehabitatHR::mcp(sp_points[, "id"], percent = level)
    mcp_sf <- sf::st_as_sf(mcp_result)
    names(mcp_sf)[1] <- "id"  # Rename for consistency
    return(sf::st_transform(mcp_sf, 4326))
  }

  if (method == "kde") {
    kde_ud <- adehabitatHR::kernelUD(sp_points, h = "href", same4all = population)
    kde_result <- adehabitatHR::getverticeshr(kde_ud, percent = level)
    kde_sf <- sf::st_as_sf(kde_result)
    names(kde_sf)[1] <- "id"
    return(sf::st_transform(kde_sf, 4326))
  }

  stop("Unsupported method: use 'mcp' or 'kde'")
}
