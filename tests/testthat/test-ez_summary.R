library(testthat)
library(ezTrack)

test_that("ez_summary returns expected columns", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  summary <- ez_summary(tracked)
  expect_s3_class(summary, "data.frame")
  expect_true(all(c("id", "n_fixes", "tracking_duration_days", "distance_km") %in% names(summary)))
})

test_that("ez_summary filters dates correctly", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  summary <- ez_summary(tracked, startDate = "2025-01-01", endDate = "2025-02-01")
  expect_s3_class(summary, "data.frame")
})

test_that("ez_summary HTML report runs", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  expect_invisible(ez_summary(tracked, report = TRUE))
})
