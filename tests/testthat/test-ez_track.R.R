library(testthat)
library(ezTrack)
library(sf)

test_that("ez_track handles data.frame input", {
  df <- data.frame(
    id = c("a", "a", "b"),
    timestamp = as.POSIXct(c("2020-01-01", "2020-01-02", "2020-01-01")),
    x = c(10, 10.1, 11),
    y = c(50, 50.1, 51)
  )
  result <- ez_track(df, as_sf = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("id", "timestamp", "x", "y") %in% names(result)))
})

test_that("ez_track handles sf input", {
  sf_obj <- st_as_sf(data.frame(
    id = "x",
    timestamp = as.POSIXct("2021-01-01"),
    x = 1,
    y = 2
  ), coords = c("x", "y"), crs = 4326)

  result <- ez_track(sf_obj)
  expect_s3_class(result, "sf")
  expect_equal(st_crs(result)$epsg, 4326)
})

test_that("ez_track can read CSV file", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    id = "abc",
    timestamp = "2020-01-01 00:00:00",
    x = 10,
    y = 20
  ), tmp, row.names = FALSE)

  result <- ez_track(tmp, as_sf = FALSE)
  expect_equal(result$id[1], "abc")
  unlink(tmp)
})

test_that("ez_track transforms to sf when as_sf = TRUE", {
  df <- data.frame(
    id = "test",
    timestamp = as.POSIXct("2021-01-01"),
    x = 100,
    y = 0
  )
  result <- ez_track(df, as_sf = TRUE)
  expect_s3_class(result, "sf")
  expect_true("geometry" %in% names(result))
})
