d <- read.csv('~/Desktop/Teampiersma HQXS 2024.csv')
d <- select(d,c(individual.local.identifier,timestamp,location.long,location.lat))

# process the data
godwit_tracks <- d %>%
  # convert timestamp to POSIXct
  mutate(timestamp = ymd_hms(timestamp)) %>%
  # round to the nearest hour
  mutate(timestamp = round_date(timestamp, unit = "hour")) %>%
  # create new id labels
  mutate(individual.local.identifier = factor(individual.local.identifier),
         individual.local.identifier = paste0("animal", as.numeric(individual.local.identifier))) %>%
  subset(individual.local.identifier != 'animal6' & individual.local.identifier != 'animal7' & individual.local.identifier != 'animal5' & individual.local.identifier != 'animal30') %>%
  distinct(timestamp, individual.local.identifier, .keep_all = TRUE) %>%
  subset(timestamp > '2025-01-01')

mapview(st_as_sf(d,coords=c('location.long','location.lat'),crs=4326))

save(godwit_tracks, file = "data/godwit_tracks.rda")

data(godwit_tracks)
tracks <- ez_track(godwit_tracks)
ez_latitude_plot(
  tracks,
  facet = TRUE,
  startDate = "2025-01-01",
  endDate = "2025-04-28",
  date_breaks = "1 month",
  date_format = "%d-%b",
)
