#' Create a clean tracking object
#'
#' Imports and standardizes tracking data into a tidy format with columns: `id`, `timestamp`, `x`, and `y`.
#' Supports input as data frames, `sf`, `sp` objects, or file paths to csv, xlsx, shp, and gpkg.
#' Optionally returns a spatial object projected to WGS84 (EPSG:4326), and supports subsampling (e.g., "1 per hour").
#'
#' @importFrom methods as
#' @importFrom stats median complete.cases
#' @importFrom utils read.csv
#' @importFrom dplyr group_by arrange slice_head ungroup select as_tibble
#'
#' @param data A tracking dataset or file path. Accepted types: `data.frame`, `sf`, `sp`, or path to "csv", "xlsx", "shp", or "gpkg".
#' @param format Optional. File format to override detection. Choices: "csv", "xlsx", "shp", "gpkg".
#' @param subsample Optional. Specify how many fixes to keep per time unit. You can use any positive integer and `"hour"` or `"day"` as the unit  (e.g.,`"1 per hour"` or `"2 per day"`).
#' @param tz Timezone for timestamps. Default is "UTC".
#' @param crs EPSG code or proj4string of the input CRS. Default is 4326 (WGS84).
#' @param as_sf Logical. Return an `sf` object? Default is TRUE.
#' @param id Optional. Column name for id.
#' @param timestamp Optional. Column name timestamp.
#' @param x Optional. Column name for longitude.
#' @param y Optional. Column name for latitude.
#' @param keep_original_cols Logical. If FALSE, drops non-standard columns and only retains `id`, `timestamp`, `x`, and `y`. Default is TRUE.
#' @param verbose Logical. Print messages? Default is TRUE.
#' @param ... Passed to the read function.
#'
#' @return A data.frame or `sf` object with columns `id`, `timestamp`, `x`, `y`.
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
                     keep_original_cols = TRUE,
                     subsample = "none",
                     verbose = TRUE,
                     ...) {

  # Null coalescing helper
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # Load tracking data depending on input type
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

      if (format %in% c("shp", "gpkg")) {
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

    if (is.data.frame(data)) return(data)

    stop("Unsupported data type: ", class(data))
  }

  # --- Load and normalize raw data ---
  df <- load_tracking_data(data, format = format, ..., verbose = verbose)

  # Normalize column names
  names(df) <- tolower(gsub("\\s+", "_", names(df)))

  # Define likely column name patterns for automated detection
  id_patterns        <- c("^id$", "individual", "individual_id", "track", "tag_id", "device", "name")
  timestamp_patterns <- c("timestamp", "datetime", "time", "date", "dt")
  x_patterns         <- c("^x$", "longitude", "lon", "location_long", "utm_e", "easting")
  y_patterns         <- c("^y$", "latitude", "lat", "location_lat", "utm_n", "northing")

  # Try to match most complete (least NA) column per pattern group
  match_column <- function(df, patterns) {
    matches <- unlist(lapply(patterns, function(p) grep(p, names(df), value = TRUE)))
    if (length(matches) == 0) return(NULL)
    scores <- sapply(matches, function(col) sum(!is.na(df[[col]])) / length(df[[col]]))
    matches[which.max(scores)]
  }

  # Guess missing columns if not explicitly provided
  id        <- id        %||% match_column(df, id_patterns)
  timestamp <- timestamp %||% match_column(df, timestamp_patterns)
  x         <- x         %||% match_column(df, x_patterns)
  y         <- y         %||% match_column(df, y_patterns)

  # Check for required columns
  missing <- c()
  if (is.null(id))        missing <- c(missing, "id")
  if (is.null(timestamp)) missing <- c(missing, "timestamp")
  if (is.null(x))         missing <- c(missing, "x")
  if (is.null(y))         missing <- c(missing, "y")
  if (length(missing) > 0)
    stop("Missing required column(s): ", paste(missing, collapse = ", "))

  if (verbose) {
    message("Detected columns - id: ", id, ", timestamp: ", timestamp, ", x: ", x, ", y: ", y)
  }

  # Rename columns to standard names
  names(df)[names(df) == id]        <- "id"
  names(df)[names(df) == timestamp] <- "timestamp"
  names(df)[names(df) == x]         <- "x"
  names(df)[names(df) == y]         <- "y"

  # Convert timestamp to POSIXct with specified timezone
  df$timestamp <- as.POSIXct(df$timestamp, tz = tz)

  # Remove rows with missing values or duplicate (id, timestamp) combinations
  n_before <- nrow(df)
  df <- df[complete.cases(df[, c("id", "timestamp", "x", "y")]), ]
  df <- df[!duplicated(df[, c("id", "timestamp")]), ]
  if (verbose && nrow(df) < n_before)
    message("Removed ", n_before - nrow(df), " row(s) with missing or duplicate values.")

  # --- Subsampling ---

  # Parse string like "2 per hour"
  parse_subsample <- function(subsample) {
    if (is.null(subsample) || subsample == "none") return(NULL)

    match <- regexec("^\\s*(\\d+)\\s+per\\s+(hour|day)\\s*$", tolower(subsample))
    parts <- regmatches(subsample, match)[[1]]

    if (length(parts) != 3)
      stop("Invalid `subsample` format. Use e.g., '1 per hour', '2 per day', or 'none'.")

    list(n = as.integer(parts[2]), unit = parts[3])
  }

  # Apply subsampling if requested
  sub <- parse_subsample(subsample)
  if (!is.null(sub)) {
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("Please install the 'dplyr' package for subsampling.")
    }

    # Create bin identifier (per id + time window)
    df$bin <- format(df$timestamp, if (sub$unit == "hour") "%Y-%m-%d %H" else "%Y-%m-%d")

    # Efficiently extract N fixes per bin
    df <- dplyr::as_tibble(df) |>
      dplyr::group_by(id, bin) |>
      dplyr::arrange(timestamp, .by_group = TRUE) |>
      dplyr::slice_head(n = sub$n) |>
      dplyr::ungroup() |>
      dplyr::select(-bin)

    df <- as.data.frame(df)

    if (verbose) message("Subsampled to ", sub$n, " fix(es) per ", sub$unit)
  }

  # --- Return as sf object if requested ---
  if (as_sf) {
    if (!requireNamespace("sf", quietly = TRUE)) stop("Please install the 'sf' package.")
    sf_obj <- sf::st_as_sf(df, coords = c("x", "y"), crs = crs, remove = FALSE)

    if (!is.na(sf::st_crs(sf_obj)$epsg) && sf::st_crs(sf_obj)$epsg != 4326) {
      if (verbose) message("Transforming to WGS84 (EPSG:4326)")
      sf_obj <- sf::st_transform(sf_obj, crs = 4326)
    }

    # Reorder to put id, timestamp, x, y first (plus geometry at end)
    other_cols <- setdiff(names(sf_obj), c("id", "timestamp", "x", "y", "geometry"))
    sf_obj <- sf_obj[, c("id", "timestamp", "x", "y", other_cols, "geometry")]

    # Reorder or trim columns before returning (sf)
    if (keep_original_cols) {
      other_cols <- setdiff(names(sf_obj), c("id", "timestamp", "x", "y", "geometry"))
      sf_obj <- sf_obj[, c("id", "timestamp", "x", "y", other_cols, "geometry")]
    } else {
      sf_obj <- sf_obj[, c("id", "timestamp", "x", "y", "geometry")]
    }

    return(sf_obj)
  }

  # Reorder to put id, timestamp, x, y first
  other_cols <- setdiff(names(df), c("id", "timestamp", "x", "y"))
  df <- df[, c("id", "timestamp", "x", "y", other_cols)]

  # Reorder or trim columns before returning
  if (keep_original_cols) {
    other_cols <- setdiff(names(df), c("id", "timestamp", "x", "y"))
    df <- df[, c("id", "timestamp", "x", "y", other_cols)]
  } else {
    df <- df[, c("id", "timestamp", "x", "y")]
  }

  return(df)
}
