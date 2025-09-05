# ezTrack

[![R-CMD-check](https://github.com/taylorbcraft/ezTrack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/taylorbcraft/ezTrack/actions/workflows/R-CMD-check.yaml)

**ezTrack** is a lightweight R package for exploring animal tracking data. It offers a gentle, beginner-friendly workflow to go from raw telemetry data to clean summaries, interactive maps, and home range estimates with minimal data wrangling. By combining data cleaning, visualization, and basic analysis in a single workflow, ezTrack lowers the barrier for students, researchers, and conservation practitioners to quickly extract ecological insight without extensive programming expertise.

[View the vignette](https://taylorbcraft.github.io/ezTrack/articles/getting-started.html)

---


## Installation

```r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("taylorbcraft/ezTrack",force = TRUE)
```

---

## Key functions

### `ez_track()` - Clean & standardize tracking data
```r
tracking_data <- ez_track("my_tracking_file.csv")
```
- Auto-detects columns for `id`, `timestamp`, `x`, and `y`
- Supports CSV, Excel, shapefiles (.shp), and GeoPackages (.gpkg) files
- Also accepts data.frame, sf, and sp objects
- Removes rows with missing or duplicate (id, timestamp) combinations
- Returns a clean data frame or (optionally) an `sf` object projected to WGS84
- Supports time-based subsampling (e.g., "1 per hour", "2 per day") 

---

### `ez_summary()` - Quick data summary
```r
ez_summary(tracking_data)
```
- Per-ID stats: duration, fixes/day, distance, speed
- Filters by date range
- Optional HTML report: `ez_summary(tracking_data, report = TRUE)`

---

### `ez_fix_rate_plot()` - Plot location fix rate
```r
ez_fix_rate_plot(tracking_data)
```
- Displays each animal’s location fixes as tick marks over time
- Helps visualize tracking effort, data gaps, and fix frequency
- Supports date filtering and x-axis formatting options
---

### `ez_home_range()` - Estimate home ranges
```r
ranges <- ez_home_range(tracking_data, method = "mcp", level = 95)
```
- Supports **MCP** and **KDE** methods
- Returns `sf` polygon(s)
- Estimate per animal or overall population

---

### `ez_map()` - Plot tracks on interactive Leaflet maps
```r
ez_map(tracking_data)
```
- Visualize tracks, points, and home ranges
- Customize colors, radius, opacity, and labels
- Toggle between satellite and light basemaps

---

### `ez_latitude_plot()` - Plot latitude over time
```r
ez_latitude_plot(tracking_data)
```
- Creates a time series plot of latitude (y-axis) over timestamp (x-axis)
- Optionally facets the plot by animal and allows customization of x-axis date format and break spacing

---

## Example Workflow
```r
library(ezTrack)

# Step 1: Import
data(godwit_tracks)

# Step 2: Standardize
trk <- ez_track(godwit_tracks)

# Step 3: Summarize
ez_summary(trk)

# Step 4: Check fix rates
ez_fix_rate_plot(trk)

# Step 5: Compute home ranges
hr <- ez_home_range(trk)

# Step 6: Map tracks and home ranges
ez_map(trk, home_ranges = hr)

# Step 7: Latitude plot
ez_latitude_plot(trk)
```

---

## Dependencies
- `sf`, `leaflet`, `geosphere`, `adehabitatHR`: spatial operations, mapping, and home range analysis  
- `readxl`: Excel import support  
- `knitr`, `kableExtra`, `htmltools`: HTML reporting  
- `ggplot2`, `viridisLite`: plotting with color palettes  
- `magrittr`: for `%>%` pipe in mapping function
- `dplyr`: subsampling and data manipulation


Install missing packages using:
```r
install.packages(c(
  "sf", "leaflet", "geosphere", "adehabitatHR", "readxl",
  "knitr", "kableExtra", "htmltools", "ggplot2", "viridisLite", 
  "magrittr", "dplyr"
))
```

---

## License
MIT License © 2025 [Taylor Craft](https://github.com/taylorbcraft)

---

## Contributing / Issues
Feel free to open an [issue](https://github.com/taylorbcraft/ezTrack/issues) or submit a pull request. Suggestions welcome!

