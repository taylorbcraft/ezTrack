## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ezTrack)

## -----------------------------------------------------------------------------
set.seed(123)

n_individuals <- 4
n_points <- 20

sample_data <- data.frame(
  animal = rep(paste0("Animal_", LETTERS[1:n_individuals]), each = n_points),
  date = rep(seq.POSIXt(
    from = as.POSIXct("2023-01-01 08:00"),
    by = "24 hours",
    length.out = n_points), n_individuals),
  location_long = runif(n_individuals * n_points, 5.5, 7),
  location_lat = runif(n_individuals * n_points, 52, 53.5)
)

head(sample_data)

tracks <- ez_track(
  data = sample_data,
  as_sf = TRUE
)

head(tracks)


## -----------------------------------------------------------------------------
# Create intentionally confusing column names
messy_data <- data.frame(
  uid = rep(c("X1", "X2"), each = 4),
  date_obs = rep(seq.POSIXt(as.POSIXct("2023-03-01 06:00"), by = "3 hours", length.out = 4), 2),
  lon_deg = runif(8, -100, -90),
  lat_deg = runif(8, 40, 45)
)

head(messy_data)

manual_cleaned <- ez_track(
  data = messy_data,
  id_col = "uid",
  time_col = "date_obs",
  x_col = "lon_deg",
  y_col = "lat_deg"
)

head(manual_cleaned)

## -----------------------------------------------------------------------------
summary_table <- ez_summary(tracks)
summary_table

## -----------------------------------------------------------------------------
ez_map(tracks)

## -----------------------------------------------------------------------------
ez_map(tracks, plotPath = TRUE,
  plotPoints = TRUE,
  pointRadius = 2,
  pointColor = "id",
  pointOpacity = 1,
  pointStroke = TRUE,
  pointStrokeColor = "black",
  pathColor = "id",
  pathWidth = 2,
  pathOpacity = 0.5,
  showLabels = TRUE)


## -----------------------------------------------------------------------------
ez_map(tracks,
  startDate = "2023-01-01",
  endDate = "2023-01-10")


## -----------------------------------------------------------------------------
home_ranges_mcp <- ez_home_range(tracks, method = 'mcp', population = TRUE)
home_ranges_kde <- ez_home_range(tracks, method = 'kde')


## -----------------------------------------------------------------------------
ez_map(home_ranges_mcp)
ez_map(home_ranges_kde, homeRangeColor = 'id')

