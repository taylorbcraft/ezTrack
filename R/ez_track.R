#' Create a Clean Tracking Object
#'
#' This function imports and standardizes tracking data into a tidy format with columns: `id`, `timestamp`, `x`, and `y`.
#' It supports common input formats including data frames, `sf` and `Spatial` objects, or file paths to CSV, Excel, Shapefiles, GeoPackages, and GeoJSON.
#' Optionally returns a spatial object projected to WGS84 (EPSG:4326).
#'
#' @importFrom methods as
#' @importFrom stats median complete.cases
#' @importFrom utils read.csv
#'
#' @param data A tracking dataset or file path. Accepted types: `data.frame`, `sf`, `Spatial*`, or a path to CSV, XLSX, SHP, GPKG, or GeoJSON.
#' @param format Optional. File format to override detection. Choices: "csv", "xlsx", "shp", "gpkg", "geojson".
#' @param tz Timezone for timestamps. Default is "UTC".
#' @param crs EPSG code or proj4string of the input CRS. Default is 4326 (WGS84).
#' @param as_sf Logical. Should the result be returned as an `sf` object? Default is TRUE.
#' @param id Column name for individual identifier. If NULL, guessed automatically.
#' @param timestamp Column name for timestamp. If NULL, guessed automatically.
#' @param x Column name for longitude/X coordinate. If NULL, guessed automatically.
#' @param y Column name for latitude/Y coordinate. If NULL, guessed automatically.
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
                     id = NULL,
                     timestamp = NULL,
                     x = NULL,
                     y = NULL,
                     verbose = TRUE,
                     ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  load_tracking_data <- function(data, format = NULL, ..., verbose = TRUE) {
    if (inherits(data, "sf")) {
      if (verbose) message("Handling input as sf object.")
      coords <- sf::st_coordinates(data)
      df <- cbind(sf::st_drop_geometry(data), coords)
      names(df)[(ncol(df) - 1):ncol(df)] <- c("x", "y")
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

  # Define robust pattern lists
  id_patterns <- c(
    "^id$", "individual", "individual_id", "individual.local.identifier", "animal", "track", "track_id",
    "trackid", "tag_id", "collar", "device", "name", "subject", "subject_id"
  )

  timestamp_patterns <- c(
    "timestamp", "date_time", "datetime", "timestamp_utc", "fix_time", "fixdate", "time", "date"
  )

  x_patterns <- c(
    "^x$", "longitude", "lon", "long", "x_coord", "location_long", 'location.long', "utm_e", "easting", "coordx"
  )

  y_patterns <- c(
    "^y$", "latitude", "lat", "y_coord", "location_lat", "location.lat","utm_n", "northing", "coordy"
  )

  # Flexible matching function
  match_column <- function(df_names, patterns) {
    matched <- unlist(lapply(patterns, function(p) grep(p, df_names, value = TRUE)))
    unique(matched)[1]
  }

  # Attempt to guess missing columns
  id        <- id        %||% match_column(names(df), id_patterns)
  timestamp <- timestamp %||% match_column(names(df), timestamp_patterns)
  x         <- x         %||% match_column(names(df), x_patterns)
  y         <- y         %||% match_column(names(df), y_patterns)

  # Detailed error if any column is missing
  missing <- c()
  if (is.null(id))        missing <- c(missing, "id")
  if (is.null(timestamp)) missing <- c(missing, "timestamp")
  if (is.null(x))         missing <- c(missing, "x")
  if (is.null(y))         missing <- c(missing, "y")

  if (length(missing) > 0) {
    stop("Could not detect required column(s): ", paste(missing, collapse = ", "),
         ". You may need to explicitly provide the column names using `id`, `timestamp`, `x`, and `y`.")
  }

  # Informative feedback
  if (verbose) {
    message("Detected columns - id: ", id, ", timestamp: ", timestamp, ", x: ", x, ", y: ", y)
  }

  # Rename columns
  names(df)[names(df) == id]        <- "id"
  names(df)[names(df) == timestamp] <- "timestamp"
  names(df)[names(df) == x]         <- "x"
  names(df)[names(df) == y]         <- "y"

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
