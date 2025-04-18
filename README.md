# ezTrack

**ezTrack** is a lightweight R package for exploring animal tracking data. It offers a gentle, beginner-friendly workflow to go from raw telemetry data to clean summaries, interactive maps, and home range estimates with minimal data wrangling.

[View the vignette](https://taylorbcraft.github.io/ezTrack/articles/getting-started.html)

---


## Installation

```r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("taylorbcraft/ezTrack")
```

---

## Key Functions

### `ez_track()` - Clean & Standardize Tracking Data
```r
clean_data <- ez_track("my_tracking_file.csv")
```
- Auto-detects columns for `id`, `timestamp`, `x`, and `y`
- Supports CSV, Excel, shapefiles (.shp), GeoPackages (.gpkg), and GeoJSON files
- Also accepts data.frame, sf, and Spatial* objects
- Removes rows with missing or duplicate (id, timestamp) combinations
- Returns a clean data frame or (optionally) a `sf` object projected to WGS84

---

### `ez_summary()` - Quick Data Summary
```r
ez_summary(clean_data)
```
- Per-ID stats: duration, fixes/day, distance, speed
- Filters by date range
- Optional HTML report: `ez_summary(data, report = TRUE)`

---

### `ez_home_range()` - Estimate Home Ranges
```r
ranges <- ez_home_range(clean_data, method = "mcp", level = 95)
```
- Supports **MCP** and **KDE** methods
- Returns `sf` polygon(s)
- Estimate per animal or overall population

---

### `ez_map()` - Interactive Mapping with Leaflet
```r
ez_map(clean_data)
```
- Visualize tracks, points, and home ranges
- Customize colors, radius, opacity, and labels
- Toggle between satellite and light basemaps

---

## Example Workflow
```r
library(ezTrack)

# Step 1: Import
trk <- ez_track("tracking.csv")

# Step 2: Summarize
ez_summary(trk)

# Step 3: Home Range
hr <- ez_home_range(trk)

# Step 4: Map It
ez_map(trk)
```

---

## Dependencies
- `sf` (spatial operations)
- `leaflet` (interactive mapping)
- `geosphere` (distance travelled calculation)
- `adehabitatHR` (home range estimation)
- `readxl` (Excel support)
- `knitr`, `kableExtra`, `htmltools` (HTML reporting)
- `ggplot2` (latitude plotting)

Install missing packages using:
```r
install.packages(c("sf", "leaflet", "geosphere", "adehabitatHR", "readxl", "knitr", "kableExtra", "htmltools"))
```

---

## License
MIT License © 2025 [Taylor Craft](https://github.com/taylorbcraft)

---

## Contributing / Issues
Feel free to open an [issue](https://github.com/taylorbcraft/ezTrack/issues) or submit a pull request. Suggestions welcome!

