# generate_test_tracks.R
# This script generates a synthetic animal tracking dataset for testing and examples.
# It saves the output to: inst/extdata/test_tracks.csv

set.seed(123)

n_animals <- 3
n_points <- 20
animal_ids <- paste0("A00", 1:n_animals)

# Assign different start times and intervals to each animal
start_times <- as.POSIXct(c("2024-01-01 06:00:00", "2024-01-02 12:00:00", "2024-01-03 18:00:00"), tz = "UTC")
intervals <- c(60, 90, 45)  # in minutes

# Generate data
test_data_multi <- do.call(rbind, lapply(seq_along(animal_ids), function(i) {
  id <- animal_ids[i]
  time_seq <- seq.POSIXt(from = start_times[i], by = paste0(intervals[i], " mins"), length.out = n_points)

  data.frame(
    ID = id,
    Timestamp = time_seq,
    Latitude = cumsum(rnorm(n_points, mean = 0.001, sd = 0.0005)) + runif(1, 52.9, 53.1),
    Longitude = cumsum(rnorm(n_points, mean = 0.001, sd = 0.0005)) + runif(1, 5.3, 5.6)
  )
}))

# Save as CSV
write.csv(test_data_multi, file = "inst/extdata/test_tracks.csv", row.names = FALSE)

# devtools::document()
# devtools::load_all()
