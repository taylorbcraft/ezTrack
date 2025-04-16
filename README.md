# ezTrack

**ezTrack** is a lightweight R package for quickly importing, summarizing, visualizing, and analyzing animal tracking data. It provides a simple workflow to go from raw GPS data to interactive maps and home range estimates with just a few lines of code.

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
- Handles CSV and Excel files
- Returns a clean data frame or `sf` object

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
- `sf`
- `leaflet`
- `geosphere`
- `adehabitatHR`
- `readxl` (for Excel support)
- `knitr`, `kableExtra`, `htmltools` (optional HTML reporting)

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

