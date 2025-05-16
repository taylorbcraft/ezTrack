library(testthat)
library(ezTrack)
library(leaflet)

test_that("ez_map returns leaflet object", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  map <- ez_map(tracked)
  expect_s3_class(map, "leaflet")
})

test_that("ez_map filters by date", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  map <- ez_map(tracked, startDate = "2025-01-01", endDate = "2025-03-01")
  expect_s3_class(map, "leaflet")
})
