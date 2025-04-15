#' Create a Clean Tracking Object
#'
#' This function imports and standardizes tracking data into a tidy format with `id`, `timestamp`, `x`, and `y`.
#' If requested, it converts the result to a spatial object using WGS84 (EPSG:4326).
#'
#' @param data Either a path to a tracking data file (CSV, Excel), or a data.frame-like object.
#' @param format Optional. File format: "csv", "xlsx". If NULL, inferred from file extension.
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
#' @return A data.frame or `sf` object with columns `id`, `timestamp`, `x`, and `y`, optionally with spatial geometry. Rows with missing values or duplicate (id, timestamp) combinations are automatically removed.

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

  if (is.data.frame(data)) {
    df <- data
    if (verbose) message("Using data.frame input directly.")
  } else {
    if (!file.exists(data)) stop("File does not exist: ", data)
    if (verbose) message("Loading file: ", data)

    if (is.null(format)) {
      format <- tolower(tools::file_ext(data))
      if (verbose) message("Inferred format: ", format)
    }

    df <- switch(format,
                 csv = read.csv(data, ...),
                 xlsx = {
                   if (!requireNamespace("readxl", quietly = TRUE)) {
                     stop("Package 'readxl' is required for reading Excel files.")
                   }
                   readxl::read_excel(data, ...)
                 },
                 stop("Unsupported format: ", format)
    )
  }

  # Clean and standardize column names
  names(df) <- tolower(gsub("\\s+", "_", names(df)))

  patterns <- c(
    "^id$",
    "^individual\\.local\\.identifier$",
    "^local\\.identifier$",
    "animal",
    "nick_name",
    "track",
    "trackId",
    "name"
  )

  id_col <- id_col %||% {
    matches <- unlist(lapply(patterns, function(p) grep(p, names(df), value = TRUE)))
    matches[1]
  }
  time_col <- time_col %||% grep("timestamp|time|date|datetime", names(df), value = TRUE)[1]
  x_col    <- x_col    %||% grep("lon|x|longitude|location_lon|location.long", names(df), value = TRUE)[1]
  y_col    <- y_col    %||% grep("lat|y|latitude|location_lat|location.lat", names(df), value = TRUE)[1]

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

  df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = tz)

  n_before <- nrow(df)
  df <- df[complete.cases(df[, c("id", "timestamp", "x", "y")]), ]
  df <- df[!duplicated(df[, c("id", "timestamp")]), ]
  n_after <- nrow(df)

  if (verbose && n_after < n_before) {
    message("Removed ", n_before - n_after, " row(s) with missing values.")
  }

  if (as_sf) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("The 'sf' package is required to return spatial objects.")
    }

    sf_obj <- sf::st_as_sf(df, coords = c("x", "y"), crs = crs, remove = FALSE)

    if (!is.na(sf::st_crs(sf_obj)$epsg) && sf::st_crs(sf_obj)$epsg != 4326) {
      if (verbose) message("Transforming to WGS84 (EPSG:4326)")
      sf_obj <- sf::st_transform(sf_obj, crs = 4326)
    }

    return(sf_obj)
  }

  return(df)
}
