library(ezTrack)

data("godwit_tracks_gambia")

godwit_ezTracks <- ez_track(godwit_tracks_gambia)

godwit_ezTracks <- subset(godwit_ezTracks,
                          id != 'C1WCGC...NL' &
                            id != 'Comporta' &
                            id != 'RBR.RY.A.' &
                            id != 'B4YYYB' &
                            id != 'Boterpol')

ez_summary(godwit_ezTracks)

ez_latitude_plot(godwit_ezTracks)

ez_map(godwit_ezTracks)

summary <- ez_summary(godwit_ezTracks)

ez_fix_rate_plot(godwit_ezTracks)

home_range <- ez_home_range(godwit_ezTracks, method = 'kde', population = TRUE)

home_range <- ez_home_range(godwit_ezTracks)


ez_map(tracks = godwit_ezTracks, home_ranges = home_range)



