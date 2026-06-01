#' Create a Clean Tracking Object
#'
#' Imports and standardizes tracking data into a tidy format with columns: `id`, `timestamp`, `x`, and `y`.
#' Supports input as data frames, `sf`, `Spatial*` objects, or file paths to CSV, Excel, Shapefiles, and GeoPackages.
#' Optionally returns a spatial object projected to WGS84 (EPSG:4326), and supports subsampling (e.g., "1 per hour").
#'
#' @importFrom methods as
#' @importFrom stats median complete.cases
#' @importFrom utils read.csv
#' @importFrom dplyr group_by arrange slice_head ungroup select as_tibble
#'
#' @param data A tracking dataset or file path. Accepted types: `data.frame`, `sf`, `Spatial*`, or path to CSV, XLSX, SHP, or GPKG.
#' @param format Optional. File format to override detection. Choices: "csv", "xlsx", "shp", "gpkg".
#' @param subsample Optional. Specify how many fixes to keep per time unit. You can use any positive integer and `"hour"` or `"day"` as the unit  (e.g.,`"1 per hour"` or `"2 per day"`).
#' @param tz Timezone for timestamps. Default is "UTC".
#' @param timestamp_format Optional. Explicit format string for parsing character
#'   timestamps, passed to `as.POSIXct(..., format = )`. If `NULL`, `ez_track()`
#'   tries a set of common datetime formats automatically.
#' @param crs EPSG code or proj4string of the input CRS. Default is 4326 (WGS84).
#' @param as_sf Logical. Return an `sf` object? Default is TRUE.
#' @param id Optional. Column name for id.
#' @param timestamp Optional. Column name for timestamp.
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
                     timestamp_format = NULL,
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

  parse_timestamp_column <- function(x, tz = "UTC", timestamp_format = NULL) {
    if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = tz))
    if (inherits(x, "POSIXlt")) return(as.POSIXct(x, tz = tz))
    if (inherits(x, "Date")) return(as.POSIXct(x, tz = tz))

    if (is.numeric(x)) {
      finite_x <- x[is.finite(x)]

      # Handle Excel serial datetimes when they are within a plausible range.
      if (length(finite_x) > 0 && all(finite_x > 20000 & finite_x < 80000)) {
        return(as.POSIXct((x - 25569) * 86400, origin = "1970-01-01", tz = tz))
      }

      stop(
        "Numeric timestamps are not supported unless they are Excel serial dates. ",
        "Convert them before calling `ez_track()` or provide a parsed datetime column."
      )
    }

    x <- as.character(x)
    x <- trimws(x)
    x[x == ""] <- NA_character_

    parse_with_formats <- function(values, formats) {
      parsed <- as.POSIXct(rep(NA_real_, length(values)), origin = "1970-01-01", tz = tz)

      for (fmt in formats) {
        needs_parse <- !is.na(values) & is.na(parsed)
        if (!any(needs_parse)) break

        attempt <- as.POSIXct(strptime(values[needs_parse], format = fmt, tz = tz))
        matched_idx <- which(needs_parse)[!is.na(attempt)]
        parsed[matched_idx] <- attempt[!is.na(attempt)]
      }

      parsed
    }

    if (!is.null(timestamp_format)) {
      parsed <- as.POSIXct(strptime(x, format = timestamp_format, tz = tz))

      if (all(is.na(parsed[!is.na(x)]))) {
        stop("Failed to parse `timestamp` using `timestamp_format = \"", timestamp_format, "\"`.")
      }

      return(parsed)
    }

    common_formats <- c(
      "%Y-%m-%d %H:%M:%OS",
      "%Y/%m/%d %H:%M:%OS",
      "%Y-%m-%d %H:%M",
      "%Y/%m/%d %H:%M",
      "%Y-%m-%dT%H:%M:%OS",
      "%Y-%m-%dT%H:%M",
      "%Y-%m-%dT%H:%M:%OSZ",
      "%Y-%m-%dT%H:%M:%OS%z",
      "%Y-%m-%d %H:%M:%OS%z",
      "%m/%d/%Y %H:%M:%OS",
      "%d/%m/%Y %H:%M:%OS",
      "%m/%d/%Y %H:%M",
      "%d/%m/%Y %H:%M",
      "%Y-%m-%d",
      "%Y/%m/%d",
      "%m/%d/%Y",
      "%d/%m/%Y",
      "%d-%m-%Y %H:%M:%OS",
      "%d-%m-%Y %H:%M",
      "%d-%m-%Y",
      "%Y%m%d %H%M%S",
      "%Y%m%d"
    )

    ambiguous_values <- x[!is.na(x) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}( \\d{1,2}:\\d{2}(:\\d{2}(\\.\\d+)?)?)?$", x)]
    if (length(ambiguous_values) > 0) {
      mdy <- parse_with_formats(ambiguous_values, c("%m/%d/%Y %H:%M:%OS", "%m/%d/%Y %H:%M", "%m/%d/%Y"))
      dmy <- parse_with_formats(ambiguous_values, c("%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%d/%m/%Y"))

      same_clock <- !is.na(mdy) & !is.na(dmy) & unclass(mdy) != unclass(dmy)
      if (any(same_clock)) {
        warning(
          "Some timestamps are ambiguous between month/day/year and day/month/year. ",
          "Set `timestamp_format` explicitly to avoid mis-parsing."
        )
      }
    }

    parsed <- parse_with_formats(x, common_formats)

    if (all(is.na(parsed[!is.na(x)]))) {
      stop(
        "Failed to parse `timestamp`. ",
        "Set `timestamp_format` explicitly if your timestamps use a non-standard format."
      )
    }

    parsed
  }

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
  if (anyDuplicated(names(df))) {
    stop("Column names are not unique after normalization. Please rename duplicate columns before calling `ez_track()`.")
  }

  score_name_match <- function(name, exact = character(), regex = character(), broad = character()) {
    score <- 0

    if (name %in% exact) {
      score <- score + 100
    }

    for (pattern in regex) {
      if (grepl(pattern, name, perl = TRUE)) {
        score <- score + 30
      }
    }

    for (pattern in broad) {
      if (grepl(pattern, name, perl = TRUE)) {
        score <- score + 10
      }
    }

    score
  }

  maybe_warn_ambiguous <- function(scores, field) {
    if (nrow(scores) < 2) return(invisible(NULL))

    scores <- scores[order(-scores$score, scores$name), ]
    top <- scores[1, ]
    runner_up <- scores[2, ]

    if (is.finite(top$score) &&
        is.finite(runner_up$score) &&
        top$score > 0 &&
        (top$score - runner_up$score) <= 8) {
      warning(
        "Auto-detection for `", field, "` is ambiguous between columns `",
        top$name, "` and `", runner_up$name, "`. Using `", top$name,
        "`. Set `", field, " = ` explicitly to override."
      )
    }
  }

  detect_id_column <- function(df) {
    exact <- c(
      "id", "animal_id", "individual_id", "tag_id", "device_id", "subject_id",
      "bird_id", "deployid", "deploy_id", "track_id", "individual.local.identifier"
    )
    regex <- c(
      "(^|_)(animal|individual|subject|bird|tag|device|deploy)(_?id)?$",
      "(^|_)track_id$",
      "(^|_)id$"
    )
    broad <- c("identifier", "individual", "animal", "subject", "bird", "tag", "device", "deploy")

    scores <- lapply(names(df), function(col) {
      values <- df[[col]]
      non_missing <- values[!is.na(values)]
      unique_ratio <- if (length(non_missing) == 0) 0 else length(unique(non_missing)) / length(non_missing)
      type_score <- if (is.character(values) || is.factor(values) || is.integer(values)) 15 else 0
      repeated_score <- if (length(non_missing) > 0 && unique_ratio > 0.01 && unique_ratio < 0.95) 15 else 0
      completeness_score <- if (length(values) == 0) 0 else 10 * (sum(!is.na(values)) / length(values))

      data.frame(
        name = col,
        score = score_name_match(col, exact, regex, broad) + type_score + repeated_score + completeness_score,
        stringsAsFactors = FALSE
      )
    })

    scores <- do.call(rbind, scores)
    scores <- scores[scores$score > 0, , drop = FALSE]
    if (nrow(scores) == 0) return(NULL)
    maybe_warn_ambiguous(scores, "id")
    scores$name[which.max(scores$score)]
  }

  detect_timestamp_column <- function(df, tz, timestamp_format) {
    exact <- c("timestamp", "datetime", "fix_time", "event_time", "time_utc", "timestamp_utc")
    regex <- c(
      "(^|_)(fix|event|obs|observation|gps|location)_(time|datetime|timestamp|date)$",
      "(^|_)(time|datetime|timestamp)$"
    )
    broad <- c("timestamp", "datetime", "time", "date")

    scores <- lapply(names(df), function(col) {
      values <- df[[col]]
      parsed_fraction <- 0

      if (inherits(values, c("POSIXct", "POSIXlt", "Date"))) {
        parsed_fraction <- 1
      } else if (is.character(values) || is.factor(values) || is.numeric(values)) {
        parsed_fraction <- tryCatch({
          parsed <- parse_timestamp_column(values, tz = tz, timestamp_format = timestamp_format)
          valid_input <- sum(!is.na(values))
          if (valid_input == 0) 0 else sum(!is.na(parsed)) / valid_input
        }, error = function(e) 0)
      }

      completeness_score <- if (length(values) == 0) 0 else 10 * (sum(!is.na(values)) / length(values))

      data.frame(
        name = col,
        score = score_name_match(col, exact, regex, broad) + 60 * parsed_fraction + completeness_score,
        parsed_fraction = parsed_fraction,
        stringsAsFactors = FALSE
      )
    })

    scores <- do.call(rbind, scores)
    scores <- scores[scores$parsed_fraction > 0 | scores$score >= 100, , drop = FALSE]
    if (nrow(scores) == 0) return(NULL)
    maybe_warn_ambiguous(scores[, c("name", "score")], "timestamp")
    scores$name[which.max(scores$score)]
  }

  detect_coord_column <- function(df, axis = c("x", "y")) {
    axis <- match.arg(axis)

    exact <- if (axis == "x") {
      c("x", "lon", "long", "longitude", "location_long", "utm_e", "easting")
    } else {
      c("y", "lat", "latitude", "location_lat", "utm_n", "northing")
    }

    regex <- if (axis == "x") {
      c("(^|_)(lon|long|longitude)$", "(^|_)(utm_?e|easting)$")
    } else {
      c("(^|_)(lat|latitude)$", "(^|_)(utm_?n|northing)$")
    }

    broad <- if (axis == "x") c("lon", "long", "easting") else c("lat", "northing")

    scores <- lapply(names(df), function(col) {
      values <- suppressWarnings(as.numeric(df[[col]]))
      numeric_fraction <- if (length(values) == 0) 0 else sum(!is.na(values)) / length(values)
      finite_values <- values[is.finite(values)]

      lon_fraction <- if (length(finite_values) == 0) 0 else mean(finite_values >= -180 & finite_values <= 180)
      lat_fraction <- if (length(finite_values) == 0) 0 else mean(finite_values >= -90 & finite_values <= 90)
      projected_fraction <- if (length(finite_values) == 0) 0 else mean(abs(finite_values) > 180)

      plausibility_score <- if (axis == "x") {
        max(20 * lon_fraction, 12 * projected_fraction)
      } else {
        max(20 * lat_fraction, 12 * projected_fraction)
      }

      variation_score <- if (length(unique(finite_values)) > 1) 10 else 0

      data.frame(
        name = col,
        score = score_name_match(col, exact, regex, broad) + 40 * numeric_fraction + plausibility_score + variation_score,
        numeric_fraction = numeric_fraction,
        stringsAsFactors = FALSE
      )
    })

    scores <- do.call(rbind, scores)
    scores <- scores[scores$numeric_fraction > 0, , drop = FALSE]
    if (nrow(scores) == 0) return(NULL)
    maybe_warn_ambiguous(scores[, c("name", "score")], axis)
    scores$name[which.max(scores$score)]
  }

  # Guess missing columns if not explicitly provided
  id        <- id        %||% detect_id_column(df)
  timestamp <- timestamp %||% detect_timestamp_column(df, tz = tz, timestamp_format = timestamp_format)
  x         <- x         %||% detect_coord_column(df, axis = "x")
  y         <- y         %||% detect_coord_column(df, axis = "y")

  # Check for required columns
  missing <- c()
  if (is.null(id))        missing <- c(missing, "id")
  if (is.null(timestamp)) missing <- c(missing, "timestamp")
  if (is.null(x))         missing <- c(missing, "x")
  if (is.null(y))         missing <- c(missing, "y")
  if (length(missing) > 0)
    stop("Missing required column(s): ", paste(missing, collapse = ", "))

  selected_cols <- c(id = id, timestamp = timestamp, x = x, y = y)
  if (length(unique(unname(selected_cols))) < length(selected_cols)) {
    stop(
      "Auto-detection mapped multiple required fields to the same source column. ",
      "Please set `id`, `timestamp`, `x`, and `y` explicitly."
    )
  }

  if (verbose) {
    message("Detected columns - id: ", id, ", timestamp: ", timestamp, ", x: ", x, ", y: ", y)
  }

  # Rename columns to standard names
  names(df)[names(df) == id]        <- "id"
  names(df)[names(df) == timestamp] <- "timestamp"
  names(df)[names(df) == x]         <- "x"
  names(df)[names(df) == y]         <- "y"

  # Parse timestamp values more defensibly than base auto-conversion alone.
  df$timestamp <- parse_timestamp_column(
    df$timestamp,
    tz = tz,
    timestamp_format = timestamp_format
  )

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
