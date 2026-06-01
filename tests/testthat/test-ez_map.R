library(testthat)
library(ezTrack)
library(leaflet)
library(sf)

test_that("ez_map returns leaflet object", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  map <- ez_map(tracked)
  expect_s3_class(map, "leaflet")
})

test_that("ez_map filters by date", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  map <- ez_map(tracked, start_date = "2025-01-01", end_date = "2025-03-01")
  expect_s3_class(map, "leaflet")
})

test_that("ez_map handles sf point objects without x/y columns", {
  tracks_sf <- st_as_sf(
    data.frame(
      id = c("a", "a", "b"),
      timestamp = as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 01:00:00", "2024-01-01 02:00:00"), tz = "UTC"),
      lon = c(10, 10.1, 11),
      lat = c(50, 50.1, 51)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  map <- ez_map(tracks_sf)
  expect_s3_class(map, "leaflet")
})
