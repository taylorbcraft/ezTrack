#' Plot Tracking Data and Home Ranges Together
#'
#' A unified function for visualizing tracking data and home range polygons on an interactive map.
#' Accepts tracking points (from `ez_track`) and home ranges (from `ez_home_range`), and
#' allows layer-specific customization of appearance and filtering by time.
#'
#' @param tracks Output from `ez_track()` — a data frame or `sf` with `id`, `timestamp`, `x`, `y`.
#' @param home_ranges Output from `ez_home_range()` — an `sf` polygon object.
#' @param individual Optional. Character or character vector of individual ID(s) to display.
#' @param show_points Logical, whether to draw individual locations. Default TRUE.
#' @param show_paths Logical, whether to draw movement paths. Default TRUE.
#' @param point_color Column name or static color for points. Default "id". Can also be "timestamp" for temporal gradient.
#' @param point_size Radius of point markers. Default 4.
#' @param point_opacity Fill opacity of points. Default 0.8.
#' @param point_stroke Logical, whether to draw point borders. Default TRUE.
#' @param point_stroke_color Border color for points. Default "black".
#' @param path_color Column name or static color for paths. Default "id".
#' @param path_width Line width of movement paths. Default 2.
#' @param path_opacity Opacity of path lines. Default 1.
#' @param polygon_color Column name or static color for home range polygons. Default "id".
#' @param polygon_opacity Fill opacity of polygons. Default 0.4.
#' @param color_palette Color palette to use for all mapped variables. Default "viridis".
#' @param show_labels Logical, whether to show hover labels for locations. Default TRUE.
#' @param start_date Optional filter (Date or string) to remove data before this date.
#' @param end_date Optional filter (Date or string) to remove data after this date.
#'
#' @return A `leaflet` map object.
#'
#' @examples
#' # Simulate tracks
#' tracks <- data.frame(
#'   id = rep(c("A", "B"), each = 10),
#'   timestamp = rep(seq.POSIXt(as.POSIXct("2023-01-01"), by = "1 day", length.out = 10), 2),
#'   x = c(runif(10, -1, 1), runif(10, 0, 2)),
#'   y = c(runif(10, 51, 52), runif(10, 51.5, 52.5))
#' )
#'
#' # Plot map
#' ez_map(tracks, point_color = "timestamp")
#' @export
ez_map <- function(tracks = NULL,
                   home_ranges = NULL,
                   individual = NULL,
                   show_points = TRUE,
                   show_paths = TRUE,
                   point_color = "id",
                   point_size = 4,
                   point_opacity = 0.8,
                   point_stroke = TRUE,
                   point_stroke_color = "black",
                   path_color = "id",
                   path_width = 2,
                   path_opacity = 1,
                   polygon_color = "id",
                   polygon_opacity = 0.4,
                   color_palette = "viridis",
                   show_labels = TRUE,
                   start_date = NULL,
                   end_date = NULL) {

  if (!requireNamespace("leaflet", quietly = TRUE)) stop("The 'leaflet' package is required.")
  if (!requireNamespace("viridisLite", quietly = TRUE)) stop("The 'viridisLite' package is required.")

  if (!is.null(individual)) {
    if (!is.null(tracks) && "id" %in% names(tracks)) {
      tracks <- tracks[tracks$id %in% individual, ]
    }
    if (!is.null(home_ranges) && "id" %in% names(home_ranges)) {
      home_ranges <- home_ranges[home_ranges$id %in% individual, ]
    }
  }

  if (!is.null(tracks)) {
    if (!inherits(tracks, "sf") && !all(c("id", "x", "y", "timestamp") %in% names(tracks))) {
      stop("`tracks` must be a data frame or sf with columns: id, x, y, timestamp")
    }
    if (!is.null(start_date)) {
      if (inherits(start_date, "character")) start_date <- as.Date(start_date)
      tracks <- tracks[tracks$timestamp >= as.POSIXct(start_date), ]
    }
    if (!is.null(end_date)) {
      if (inherits(end_date, "character")) end_date <- as.Date(end_date)
      tracks <- tracks[tracks$timestamp <= as.POSIXct(end_date + 1) - 1, ]
    }

    # Precompute point color
    if (point_color %in% names(tracks)) {
      if (point_color == "timestamp") {
        pal <- leaflet::colorNumeric("Reds", domain = as.numeric(range(tracks$timestamp, na.rm = TRUE)))
        tracks$point_col <- pal(as.numeric(tracks$timestamp))
      } else {
        pal <- leaflet::colorFactor(color_palette, domain = unique(tracks[[point_color]]))
        tracks$point_col <- pal(tracks[[point_color]])
      }
    } else {
      tracks$point_col <- point_color
    }

    # Precompute path color
    if (path_color %in% names(tracks)) {
      if (path_color == "timestamp") {
        pal <- leaflet::colorNumeric("Reds", domain = as.numeric(range(tracks$timestamp, na.rm = TRUE)))
        tracks$path_col <- pal(as.numeric(tracks$timestamp))
      } else {
        pal <- leaflet::colorFactor(color_palette, domain = unique(tracks[[path_color]]))
        tracks$path_col <- pal(tracks[[path_color]])
      }
    } else {
      tracks$path_col <- path_color
    }
  }

  map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Light") %>%
    leaflet::addLayersControl(
      baseGroups = c("Satellite", "Light"),
      overlayGroups = c("Tracks", "Polygons"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  if (!is.null(tracks)) {
    ids <- unique(tracks$id)

    for (i in seq_along(ids)) {
      sub <- tracks[tracks$id == ids[i], ]

      if (show_paths) {
        map <- map %>%
          leaflet::addPolylines(data = sub,
                                lng = ~x, lat = ~y,
                                color = sub$path_col[1],
                                weight = path_width,
                                opacity = path_opacity,
                                group = "Tracks")
      }

      if (show_points) {
        labels <- if (show_labels) {
          lapply(seq_len(nrow(sub)), function(j) {
            htmltools::HTML(paste0(
              "ID: ", sub$id[j],
              "<br>Time: ", format(sub$timestamp[j], "%Y-%m-%d %H:%M:%S")
            ))
          })
        } else NULL

        map <- map %>%
          leaflet::addCircleMarkers(data = sub,
                                    lng = ~x, lat = ~y,
                                    radius = point_size,
                                    fillColor = sub$point_col,
                                    color = point_stroke_color,
                                    fillOpacity = point_opacity,
                                    stroke = point_stroke,
                                    weight = 1,
                                    label = labels,
                                    group = "Tracks")
      }
    }
  }

  if (!is.null(home_ranges)) {
    if (!inherits(home_ranges, "sf")) stop("`home_ranges` must be an sf object.")

    geom_type <- unique(sf::st_geometry_type(home_ranges))
    if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
      warning("`home_ranges` is not a polygon layer.")
    } else {
      poly_col <- if (is.character(polygon_color) && polygon_color %in% names(home_ranges)) {
        pal <- leaflet::colorFactor(color_palette, domain = home_ranges[[polygon_color]])
        pal(home_ranges[[polygon_color]])
      } else polygon_color

      map <- map %>%
        leaflet::addPolygons(data = home_ranges,
                             color = poly_col,
                             weight = 2,
                             fillOpacity = polygon_opacity,
                             label = ~as.character(id),
                             group = "Polygons")
    }
  }

  return(map)
}
