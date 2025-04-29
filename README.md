# QuantAQ Air Quality Data Analysis and Dashboard

This repository contains scripts, data, and a Shiny dashboard related to the analysis of air quality data collected with QuantAQ devices across Ghana and other locations.

## Project Structure
- `analysis/` – Analysis scripts and outputs (e.g., trends, completeness)
- `archive/` – Archived reports, scripts, and older datasets
- `air_pollution_dashboard/` – Shiny dashboard application: https://cumc-columbia-air-pollution-dashboard.shinyapps.io/cumc-air-pollution-dashboard/
- `data/` – Organized raw and processed datasets
- `data_load_and_prep/` – Scripts for data cleaning and preparation
- `plots/` – Figures and visualizations
- `src/` – Core functions and utilities
- `QuantAQ.Rproj` – RStudio project file

## Repository Components

### 1. Analysis
Contains R scripts and output files for:
- **Data Completeness** analysis comparing SD card and cloud sources
- **Pollution Trends** (e.g., seasonal patterns, Harmattan effects)

### 2. Air Pollution Dashboard
An interactive Shiny dashboard for visualizing:
- Time series of pollution readings
- Monitor-level comparisons
- Fleet-wide averages
- Correlations between pollutants

Dashboard located at:  
`air_pollution_dashboard/app/`  
Deployment details in:  
`air_pollution_dashboard/app/rsconnect/`

### 3. Data
Raw and processed datasets, organized into:
- `pm/` — Particulate matter (PM1, PM2.5, PM10)
- `gas/` — Gaseous pollutants (CO, NO, NO2, O3)
- `weather/` — Temperature, humidity, wind data
- `spatial/` — Ghana shapefiles and road network layers
- `missingness/` — Metadata tracking missing data over time

> ⚡ **Note:** Data is stored securely offline.

### 4. Data Load and Preparation
Scripts for:
- Loading datasets from Box/other cloud storage
- Cleaning BAM reference monitor data
- Processing temperature, humidity, and wind sensor data

### 5. Plots
Saved figures for reports and presentations:
- `pm_plots/` — PM trends and maps
- `gas_plots/` — Gas concentration visualizations
- `wind_plots/` — Wind rose plots

### 6. Source Code (`src/`)
Functions for:
- Merging cloud and SD card data
- Creating spatial plots
- Running regressions with road density and pollution
- Summarizing pollution times and completeness
