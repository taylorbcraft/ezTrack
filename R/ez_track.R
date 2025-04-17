#' Create a Clean Tracking Object
#'
#' This function imports and standardizes tracking data into a tidy format with columns: `id`, `timestamp`, `x`, and `y`.
#' It supports common input formats including data frames, `sf` and `Spatial` objects, or file paths to CSV, Excel, Shapefiles, GeoPackages, and GeoJSON.
#' Optionally returns a spatial object projected to WGS84 (EPSG:4326).
#'
#' @param data A tracking dataset or file path. Accepted types: `data.frame`, `sf`, `Spatial*`, or a path to CSV, XLSX, SHP, GPKG, or GeoJSON.
#' @param format Optional. File format to override detection. Choices: "csv", "xlsx", "shp", "gpkg", "geojson".
#' @param tz Timezone for timestamps. Default is "UTC".
#' @param crs EPSG code or proj4string of the input CRS. Default is 4326 (WGS84).
#' @param as_sf Logical. Should the result be returned as an `sf` object? Default is TRUE.
#' @param id_col Name of the column to use as ID. If NULL, guessed automatically.
#' @param time_col Name of the timestamp column. If NULL, guessed automatically.
#' @param x_col Name of the longitude/X column. If NULL, guessed automatically.
#' @param y_col Name of the latitude/Y column. If NULL, guessed automatically.
#' @param verbose Logical. If TRUE, print details during import. Default is TRUE.
#' @param ... Additional arguments passed to the read function.
#'
#' @return A data.frame or `sf` object with standardized columns `id`, `timestamp`, `x`, and `y`. Missing values and duplicate (id, timestamp) rows are removed.
#' @export

ez_track <- function(data,
                     format = NULL,
                     tz = "UTC",
                     crs = 4326,
                     as_sf = TRUE,
                     id_col = NULL,
                     time_col = NULL,
                     x_col = NULL,
                     y_col = NULL,
                     verbose = TRUE,
                     ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  load_tracking_data <- function(data, format = NULL, ..., verbose = TRUE) {
    if (inherits(data, "sf")) {
      if (verbose) message("Handling input as sf object.")
      coords <- sf::st_coordinates(data)
      df <- cbind(sf::st_drop_geometry(data), coords)
      names(df)[(ncol(df)-1):ncol(df)] <- c("x", "y")
      return(df)
    }

    if (inherits(data, "Spatial")) {
      if (verbose) message("Handling input as Spatial object.")
      df <- as.data.frame(data)
      coords <- sp::coordinates(data)
      df$x <- coords[, 1]
      df$y <- coords[, 2]
      return(df)
    }

    if (is.character(data)) {
      if (!file.exists(data)) stop("File does not exist: ", data)
      if (is.null(format)) format <- tolower(tools::file_ext(data))
      if (verbose) message("Detected file format: ", format)

      if (format %in% c("shp", "gpkg", "geojson", "json", "kml")) {
        if (!requireNamespace("sf", quietly = TRUE)) stop("Please install the 'sf' package.")
        return(sf::st_read(data, quiet = !verbose, ...))
      }

      if (format == "csv") return(read.csv(data, ...))
      if (format == "xlsx") {
        if (!requireNamespace("readxl", quietly = TRUE)) stop("Please install the 'readxl' package.")
        return(readxl::read_excel(data, ...))
      }

      stop("Unsupported file format: ", format)
    }

    if (is.data.frame(data)) {
      return(data)
    }

    stop("Unsupported data type: ", class(data))
  }

  df <- load_tracking_data(data, format = format, ..., verbose = verbose)

  # Standardize column names
  names(df) <- tolower(gsub("\\s+", "_", names(df)))

  id_col <- id_col %||% {
    patterns <- c("^id$", "individual.*identifier", "animal", "track", "trackid", "nick_name", "name")
    matches <- unlist(lapply(patterns, function(p) grep(p, names(df), value = TRUE)))
    matches[1]
  }

  time_col <- time_col %||% grep("timestamp|date|time|datetime", names(df), value = TRUE)[1]
  x_col    <- x_col    %||% grep("lon|x|longitude", names(df), value = TRUE)[1]
  y_col    <- y_col    %||% grep("lat|y|latitude", names(df), value = TRUE)[1]

  if (!all(c(id_col, time_col, x_col, y_col) %in% names(df))) {
    stop("Could not identify required columns: id, timestamp, x, and y.")
  }

  if (verbose) {
    message("Columns detected:")
    message("id: ", id_col)
    message("time: ", time_col)
    message("x: ", x_col)
    message("y: ", y_col)
  }

  names(df)[names(df) == id_col]   <- "id"
  names(df)[names(df) == time_col] <- "timestamp"
  names(df)[names(df) == x_col]    <- "x"
  names(df)[names(df) == y_col]    <- "y"

  df$timestamp <- as.POSIXct(df$timestamp, tz = tz)

  n_before <- nrow(df)
  df <- df[complete.cases(df[, c("id", "timestamp", "x", "y")]), ]
  df <- df[!duplicated(df[, c("id", "timestamp")]), ]
  n_after <- nrow(df)

  if (verbose && n_after < n_before) {
    message("Removed ", n_before - n_after, " row(s) with missing values.")
  }

  if (as_sf) {
    if (!requireNamespace("sf", quietly = TRUE)) stop("Please install the 'sf' package.")
    sf_obj <- sf::st_as_sf(df, coords = c("x", "y"), crs = crs, remove = FALSE)

    if (!is.na(sf::st_crs(sf_obj)$epsg) && sf::st_crs(sf_obj)$epsg != 4326) {
      if (verbose) message("Transforming to WGS84 (EPSG:4326)")
      sf_obj <- sf::st_transform(sf_obj, crs = 4326)
    }

    return(sf_obj)
  }

  return(df)
}
