############################################################
# Supplementary Code S1 – Manual versus ezTrack workflow
############################################################

############################################################
#### Data import and cleaning                           ####
# Equivalent ezTrack implementation:
# godwit_tracks <- ez_track(godwit_tracks_gambia)
############################################################

library(ggplot2)

# Import raw data
data("godwit_tracks_gambia")

# Keep only necessary columns
godwit_tracks <- godwit_tracks_gambia[, c("id", "timestamp", "x", "y")]

# Parse timestamp
godwit_tracks$timestamp <- as.POSIXct(godwit_tracks$timestamp, tz = "UTC")

# Remove rows with missing values
godwit_tracks <- godwit_tracks[!is.na(godwit_tracks$id) &
                                 !is.na(godwit_tracks$timestamp) &
                                 !is.na(godwit_tracks$x) &
                                 !is.na(godwit_tracks$y), ]

# Sort by id and timestamp
godwit_tracks <- godwit_tracks[order(godwit_tracks$id, godwit_tracks$timestamp), ]

# Remove duplicate locations
godwit_tracks <- godwit_tracks[!duplicated(paste(godwit_tracks$id, godwit_tracks$timestamp)), ]


############################################################
#### Deployment timeline plot                           ####
# Equivalent ezTrack implementation:
# ez_fix_rate_plot(godwit_tracks)
############################################################

# Create deployment start and end per individual
ranges <- aggregate(timestamp ~ id, data = godwit_tracks,
                    FUN = function(x) c(start = min(x), end = max(x)))
ranges <- do.call(data.frame, ranges)
names(ranges) <- c("id", "animal_timestamp_start", "animal_timestamp_end")

# Parse timestamps
ranges$animal_timestamp_start <- as.POSIXct(ranges$animal_timestamp_start,
                                            origin = "1970-01-01", tz = "UTC")
ranges$animal_timestamp_end <- as.POSIXct(ranges$animal_timestamp_end,
                                          origin = "1970-01-01", tz = "UTC")

# Order individuals by end date
ranges <- ranges[order(ranges$animal_timestamp_end, decreasing = TRUE), ]

# Create ordered factor for plotting
godwit_tracks$y_labels <- factor(godwit_tracks$id, levels = ranges$id)
ranges$y_labels <- factor(ranges$id, levels = ranges$id)

# Plot deployment span and fix rate
ggplot(ranges, aes(x = animal_timestamp_start,
                   xend = animal_timestamp_end,
                   y = y_labels,
                   yend = y_labels)) +
  geom_segment(color = "black", linewidth = 0.8) +
  geom_point(data = godwit_tracks,
             inherit.aes = FALSE,
             aes(x = timestamp, y = y_labels),
             shape = 124,
             size = 3,
             color = "blue") +
  scale_x_datetime(date_breaks = "1 year",
                   date_labels = "%b %Y") +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


############################################################
#### Home range estimation and plotting (95% KDE) ####
# Equivalent ezTrack implementation:
# home_range <- ez_home_range(godwit_tracks, population = TRUE, method = "kde")
# ez_map(home_ranges = home_range)
############################################################

library(sf)
library(adehabitatHR)
library(sp)

# Convert to sf and project to metric CRS
godwit_tracks_sf <- st_as_sf(godwit_tracks, coords = c("x", "y"), crs = 4326)
godwit_tracks_utm <- st_transform(godwit_tracks_sf, 32628)

# Convert to SpatialPointsDataFrame and treat all fixes as one population
pop_sp <- as(godwit_tracks_utm, "Spatial")
pop_sp@data <- data.frame(group = factor(rep("population", nrow(godwit_tracks))))

# Estimate KDE and extract 95% isopleth
kud_pop <- kernelUD(pop_sp[, "group"], h = "href", grid = 200)
kde95 <- getverticeshr(kud_pop, percent = 95)

kde95_sf <- st_as_sf(kde95)
kde95_sf$level <- 95

# Plot KDE isopleth and fixes
ggplot() +
  geom_sf(data = kde95_sf, aes(fill = factor(level)), alpha = 0.25, color = NA) +
  geom_sf(data = godwit_tracks_utm, size = 0.6, alpha = 0.25) +
  labs(fill = "KDE level", x = NULL, y = NULL) +
  theme_bw()
