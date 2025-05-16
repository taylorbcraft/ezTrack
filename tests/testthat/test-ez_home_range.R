library(testthat)
library(ezTrack)
library(sf)

test_that("ez_home_range returns sf object", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  range <- ez_home_range(tracked)
  expect_s3_class(range, "sf")
  expect_true("id" %in% names(range))
})

test_that("ez_home_range supports KDE method", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  range <- ez_home_range(tracked, method = "kde")
  expect_s3_class(range, "sf")
})

test_that("ez_home_range filters dates", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  range <- ez_home_range(tracked, startDate = "2025-01-01", endDate = "2025-03-01")
  expect_s3_class(range, "sf")
})
