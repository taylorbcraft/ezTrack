#' Interactive Map of Tracking Data
#'
#' Display animal tracks on an interactive Leaflet map with intuitive styling arguments.
#' Optionally subset the data by a start and end date. Supports plotting tracks (lines and points)
#' and home ranges (POLYGON or MULTIPOLYGON). By default, points and paths are colored by `id`.
#'
#' @param data A data frame or `sf` object with columns `id`, `timestamp`, `x`, and `y`.
#' @param plotPath Logical, whether to draw path lines. Default is TRUE.
#' @param plotPoints Logical, whether to draw points. Default is TRUE.
#' @param pointRadius Numeric, size of point markers. Default is 4.
#' @param pointColor CSS color string or column name to color points. Default is `"id"`.
#' @param pointOpacity Numeric (0–1), fill opacity for points. Default is 0.8.
#' @param pointStroke Logical, whether to draw borders around points. Default is TRUE.
#' @param pointStrokeColor Border color for points. Default is "black".
#' @param pathColor CSS color string or column name to color paths. Default is `"id"`.
#' @param pathWidth Line width of paths. Default is 2.
#' @param pathOpacity Numeric (0–1), opacity of paths. Default is 1.
#' @param homeRangeColor CSS color string or column name to color polygon geometries. Default is "blue".
#' @param colorPalette Palette to use if coloring by `id` or another column. Supports "viridis", "plasma", "inferno", "magma", "cividis", "turbo", or "Set1". Default is "viridis".
#' @param showLabels Logical, show ID + timestamp in hover labels. Default is TRUE.
#' @param startDate Optional. A `Date` object or a date string (e.g., "2021-01-01").
#' @param endDate Optional. A `Date` object or a date string (e.g., "2021-12-31").
#'
#' @return A `leaflet` map object.
#' @export
#' @importFrom magrittr %>%
ez_map <- function(data,
                   plotPath = TRUE,
                   plotPoints = TRUE,
                   pointRadius = 4,
                   pointColor = "blue",
                   pointOpacity = 0.8,
                   pointStroke = TRUE,
                   pointStrokeColor = "black",
                   pathColor = "blue",
                   pathWidth = 2,
                   pathOpacity = 1,
                   homeRangeColor = "blue",
                   colorPalette = "viridis",
                   showLabels = TRUE,
                   startDate = NULL,
                   endDate = NULL) {

  if (!requireNamespace("leaflet", quietly = TRUE)) stop("The 'leaflet' package is required. Please install it.")
  if (!requireNamespace("viridisLite", quietly = TRUE)) stop("The 'viridisLite' package is required. Please install it.")

  # Handle polygon data
  if (inherits(data, "sf") && all(sf::st_geometry_type(data) %in% c("POLYGON", "MULTIPOLYGON"))) {
    poly_col <- if (is.character(homeRangeColor) && homeRangeColor %in% names(data)) {
      pal <- leaflet::colorFactor("Set2", domain = data[[homeRangeColor]])
      pal(data[[homeRangeColor]])
    } else {
      homeRangeColor
    }

    return(
      leaflet::leaflet(data) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addPolygons(
          color = poly_col,
          weight = 2,
          fillOpacity = 0.4,
          label = ~as.character(id)
        )
    )
  }

  if (!all(c("id", "x", "y", "timestamp") %in% names(data))) {
    stop("Input data must include columns: 'id', 'x', 'y', and 'timestamp'")
  }

  # Filter by date
  if (!is.null(startDate)) {
    if (inherits(startDate, "character")) startDate <- as.Date(startDate)
    data <- data[data$timestamp >= as.POSIXct(startDate), ]
  }

  if (!is.null(endDate)) {
    if (inherits(endDate, "character")) endDate <- as.Date(endDate)
    data <- data[data$timestamp <= as.POSIXct(endDate + 1) - 1, ]
  }

  # Default coloring by id
  if (pointColor == "blue") pointColor <- "id"
  if (pathColor == "blue") pathColor <- "id"

  # Init map
  map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Light") %>%
    leaflet::addLayersControl(
      baseGroups = c("Satellite", "Light"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  ids <- unique(data$id)

  get_palette <- function(palette_name, domain) {
    if (palette_name %in% c("viridis", "plasma", "inferno", "magma", "cividis", "turbo")) {
      leaflet::colorFactor(viridisLite::viridis(length(unique(domain)), option = palette_name), domain = domain)
    } else {
      suppressWarnings(leaflet::colorFactor(palette_name, domain = domain))
    }
  }

  shared_palette <- NULL
  use_shared_palette <- pointColor == pathColor && pointColor %in% names(data)
  if (use_shared_palette) {
    shared_palette <- get_palette(colorPalette, data[[pointColor]])
  }

  for (i in seq_along(ids)) {
    sub <- data[data$id == ids[i], ]

    path_col <- if (is.character(pathColor) && pathColor %in% names(data)) {
      if (use_shared_palette) shared_palette(sub[[pathColor]][1])
      else get_palette(colorPalette, data[[pathColor]])(sub[[pathColor]][1])
    } else {
      pathColor
    }

    point_col <- if (is.character(pointColor) && pointColor %in% names(data)) {
      if (use_shared_palette) shared_palette(sub[[pointColor]][1])
      else get_palette(colorPalette, data[[pointColor]])(sub[[pointColor]][1])
    } else {
      pointColor
    }

    if (plotPath) {
      map <- map %>%
        leaflet::addPolylines(
          data = sub,
          lng = ~x, lat = ~y,
          color = path_col,
          weight = pathWidth,
          opacity = pathOpacity,
          group = ids[i]
        )
    }

    if (plotPoints) {
      labels <- if (showLabels) {
        lapply(seq_len(nrow(sub)), function(j) {
          htmltools::HTML(paste0(
            "ID: ", sub$id[j],
            "<br>Time: ", format(sub$timestamp[j], "%Y-%m-%d %H:%M:%S")
          ))
        })
      } else {
        NULL
      }

      map <- map %>%
        leaflet::addCircleMarkers(
          data = sub,
          lng = ~x, lat = ~y,
          radius = pointRadius,
          fillColor = point_col,
          color = pointStrokeColor,
          fillOpacity = pointOpacity,
          stroke = pointStroke,
          weight = 1,
          label = labels,
          group = ids[i],
          opacity = 1
        )
    }
  }

  return(map)
}
