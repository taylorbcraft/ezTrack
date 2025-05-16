library(testthat)
library(ezTrack)
library(ggplot2)

test_that("ez_latitude_plot returns ggplot object", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  plot <- ez_latitude_plot(tracked)
  expect_s3_class(plot, "ggplot")
})

test_that("ez_latitude_plot facets correctly", {
  data(godwit_tracks)
  tracked <- ez_track(godwit_tracks)
  plot <- ez_latitude_plot(tracked, facet = TRUE)
  expect_s3_class(plot, "ggplot")
})
