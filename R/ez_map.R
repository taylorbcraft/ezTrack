#' Interactive Map of Tracking Data
#'
#' Display animal tracks on an interactive Leaflet map with intuitive styling arguments.
#'
#' @param data A data frame or `sf` object with columns `id`, `timestamp`, `x`, and `y`.
#' @param plotPath Logical, whether to draw path lines. Default is TRUE.
#' @param plotPoints Logical, whether to draw points. Default is TRUE.
#' @param pointRadius Numeric, size of point markers. Default is 4.
#' @param pointColor CSS color string or column name to color points. Default is "blue".
#' @param pointOpacity Numeric (0–1), fill opacity for points. Default is 0.8.
#' @param pointStroke Logical, whether to draw borders around points. Default is TRUE.
#' @param pointStrokeColor Border color for points. Default is "black".
#' @param pathColor CSS color string or column name to color paths. Default is "blue".
#' @param pathWidth Line width of paths. Default is 2.
#' @param pathOpacity Numeric (0–1), opacity of paths. Default is 1.
#' @param showLabels Logical, show ID + timestamp in hover labels. Default is TRUE.
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
                       showLabels = TRUE) {

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The 'leaflet' package is required for map_tracks(). Please install it.")
  }

  if (!all(c("id", "x", "y", "timestamp") %in% names(data))) {
    stop("Input data must include columns: 'id', 'x', 'y', and 'timestamp'")
  }

  map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    leaflet::addProviderTiles("CartoDB.Positron", group = "Light") %>%
    leaflet::addLayersControl(
      baseGroups = c("Satellite", "Light"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  ids <- unique(data$id)

  for (i in seq_along(ids)) {
    sub <- data[data$id == ids[i], ]

    # Dynamic path color
    path_col <- if (is.character(pathColor) && pathColor %in% names(data)) {
      pal <- leaflet::colorFactor("Set1", domain = data[[pathColor]])
      pal(sub[[pathColor]][1])
    } else {
      pathColor
    }

    # Dynamic point color
    point_col <- if (is.character(pointColor) && pointColor %in% names(data)) {
      pal <- leaflet::colorFactor("Set1", domain = data[[pointColor]])
      pal(sub[[pointColor]][1])
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
