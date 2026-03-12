# Jamaica Bay Wildlife Refuge - Wind Data Dashboard

Interactive Shiny dashboard displaying environmental sensor data from the RM-0002 weather station at West Pond, Jamaica Bay Wildlife Refuge, NY.

**Live App:** [https://jjherlan.github.io/jamaica_bay_wind_data/](https://jjherlan.github.io/jamaica_bay_wind_data/)

## About

This project analyzes wind-driven wave erosion effects on smooth cordgrass (*Spartina alterniflora*) restoration in Jamaica Bay. The RM-0002 sensor was installed on September 18, 2025 and collects wind speed, wind direction, air temperature, humidity, and barometric pressure at approximately 1-3 minute intervals.

**PIs:** Phillip P.A. Staniczenko & Chester Zarnoch, CUNY

## Dashboard Features

- **Time Series** - Wind speed, direction, and temperature with GAM trend lines
- **Wind Rose** - Directional wind frequency diagrams (overall, monthly, day/night)
- **Monthly Boxplots** - Distribution comparisons across months
- **Summary Statistics** - Monthly and directional breakdowns

## Data

The dashboard includes 192,713 observations spanning August 2025 through March 2026 from 7 collection periods. The pre-processed dataset (`jamaica_bay_clean.csv`) contains 6 variables: local_time, wind direction (degrees), humidity (%), pressure (mb), wind speed (m/s), and temperature (C).

## Running Locally

To run the app in RStudio:

```r
# Install required packages
install.packages(c("shiny", "ggplot2", "dplyr", "lubridate", "tidyr", "mgcv"))

# Run the app
shiny::runApp()
```

## Deployment

This app is deployed as a [Shinylive](https://posit-dev.github.io/r-shinylive/) application on GitHub Pages via GitHub Actions. The app runs entirely in your browser using WebAssembly (no server required).

## License

Data and code for research and educational purposes.
