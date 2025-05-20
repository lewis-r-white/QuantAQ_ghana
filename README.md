# QuantAQ Air Quality Data Analysis and Dashboard

This repository contains scripts, datasets, and a Shiny dashboard related to air quality monitoring using QuantAQ devices across the Bono East region of Ghana.

## Project Structure

- `analysis/` – Analysis scripts and outputs (e.g., trends, completeness)
- `archive/` – Archived reports, scripts, and earlier datasets
- `air_pollution_dashboard/` – Shiny dashboard app:  
  [View Dashboard](https://cumc-columbia-air-pollution-dashboard.shinyapps.io/cumc-air-pollution-dashboard/)
- `data/` – **Note:** Not included in GitHub, but contains raw, processed, and summarized datasets.
- `data_load_and_prep/` – Scripts for data ingestion and cleaning
- `plots/` – Figures used in reports and visualizations
- `src/` – Functions used in cleaning, merging, and analysis
- `QuantAQ.Rproj` – RStudio project file

The sections below describe the workflow used to **load, clean, and analyze** air quality data from QuantAQ MODULAIR and MODULAIR-PM devices.

---

## 1. Data Access and Loading Cloud + SD Card Data

### Key file: [`load_ghana_AQ_data.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/data_load_and_prep/load_ghana_AQ_data.Rmd)

This script walks through the process of loading and preparing data from:

- QuantAQ's loud-based API
- SD card exports processed by the QuantAQ team

### Loading Cloud Data via API

To download data directly from the QuantAQ cloud:

#### Requirements

- A QuantAQ account with developer access
- A valid API key (generate under `Developer > API Keys` on the QuantAQ dashboard)
- Access to the specific devices you want to query

#### Workflow Steps (in `load_ghana_AQ_data.Rmd`)

1. Load necessary packages (e.g., `QuantAQAPIClient`, `purrr`, `lubridate`, etc.)
2. Run `setup_client()` and enter your API key when prompted
3. Create a list of device serial numbers (e.g., `"MOD-00397"`, `"MOD-PM-00893"`)
4. Define the start and end dates for the download
5. Use a loop to download minutely data for each day and each device
6. The script standardizes timestamps, adds metadata (`monitor`, `date`, `hour`), and merges all data into a single file

#### Output

Saved output files follow this format: /data/all_measurements/cloud/ghana_AQ_parent_full_YYYYMMDD-YYYYMMDD.csv

### Loading SD Card Data

Before use, raw SD card files must be processed by QuantAQ. This ensures formatting is consistent with cloud data (i.e., aligned to minutely intervals).

#### Preparing Data for Upload

1. Create a subfolder for each monitor using the serial number as the folder name (e.g., `MOD-00397`)
2. Place all raw `.csv` files from that device's SD card into the subfolder
3. Zip the folder containing all monitor subfolders into a single `.zip` file
4. Open a support ticket with QuantAQ and upload the `.zip` for processing

#### Processed Output

QuantAQ will return cleaned `.csv` files, one per device. Each `.csv` is named using the monitor ID, e.g., `MOD-00397.final.csv`. Place these files into: 
- /data/all_measurements/sd/processed_<DATE>/MOD
- /data/all_measurements/sd/processed_<DATE>/MOD-PM

#### Workflow: The file `load_ghana_AQ_data.Rmd` includes:

- Functions to extract `timestamp_iso`, pollutant concentrations, and metadata
- Automatic parsing of monitor names from file paths
- Standardization of column names to align with cloud data

This ensures cloud and SD card data can be merged consistently for analysis.


## 2. Merging Cloud and SD Card Data (PM, Gas, and Weather)

### Key files: 
### - [`pollutant_data_cleaning.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/data_load_and_prep/pollutant_data_cleaning.Rmd)
### - [`weather_data_cleaning.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/data_load_and_prep/weather_data_cleaning.Rmd)

Once the cloud and SD card datasets are loaded, they are merged to create a complete time series per monitor. This ensures data continuity even during periods when cloud data is missing due to connectivity issues or device syncing delays. For more on missing data, see section 5.1: Missing Data Analysis in this ReadMe file. 

### Workflow Overview

For each pollutant or weather variable (e.g., `pm25`, `co`, `temp`, etc.):

1. Cloud data is loaded and split into:
   - Colocation period (e.g., `2023-08-16` to `2023-09-20`)
   - Community deployment period (e.g., `2023-09-26` onward)

2. SD card data is loaded (see section 1).

3. Cloud and SD card data are merged using the `merge_cloud_sd_colocation_and_community()` function:
   - If cloud data is present, it is prioritized.
   - If cloud data is missing, the corresponding SD card value is used.
   - A `source` column is used to track whether each value came from `cloud` or `sd_card`.

4. The result is a list of merged dataframes per pollutant:
   - `merged_full`: all time periods
   - `merged_colocation`: colocated calibration period only
   - `merged_community`: community deployment period only

### Key Functions

- `load_pollution_datasets()`  
  Loads cloud data from `.csv` or `.rds`, filters extreme values (e.g. PM10 > 1500), and separates it into colocated and community periods.

- `merge_sd_data()`  
  Joins SD card data to cloud data by timestamp and monitor ID. Uses `coalesce()` to prefer cloud values and fills gaps using SD card data.

- `merge_cloud_sd_colocation_and_community()`  
  Applies the merging logic above across multiple pollutants, returning a structured list for analysis.

### Output Format
Each pollutant gets a named list of merged dataframes, such as:
- merged_results$pm25$merged_full
- merged_results$pm25$merged_colocation
- merged_results$pm25$merged_community

These merged datasets are saved to:
- /data/pm/raw/ for particulate matter (PM1, PM2.5, PM10)
- /data/gas/raw/ for gaseous pollutants (CO, NO, NO2, O3)
- /data/weather/merged/ for environmental variables (temp, RH, wind)

For the pollutants (PMs and gases), the merged data is then passed to calibration functions (see Section 3 below).

## 3. Calibration and Correction of Pollutant Measurements

### Key file: [`pollutant_data_cleaning.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/data_load_and_prep/pollutant_data_cleaning.Rmd)

After merging cloud and SD card data (Section 2), calibration is applied to correct for systematic biases across monitors. **Note**: The approach differs for PM and gas measurements.

### PM Calibration: Colocation-Based Regression

For particulate matter (PM1, PM2.5, PM10), calibration is based on colocation data collected when all monitors were stationed together.

**Steps:**

1. **Select Colocation Data**  
   Filter data to the colocation period (e.g., `2023-08-16` to `2023-09-20`) using `merged_results$<pollutant>$merged_colocation`.

2. **Calculate Fleet Average**  
   For each timestamp, compute the average value across all active monitors.

3. **Filter Timestamps**  
   Only include timestamps where at least 10 monitors were active, to ensure stability of the fleet average.

4. **Run Regressions**  
   For each monitor, run a linear regression comparing its readings to the fleet average. Store the slope, intercept, RMSE, and MAE. The function `apply_regression()` in [`compare_fleet_regression.R`](https://github.com/lewis-r-white/QuantAQ/blob/main/src/compare_fleet_regression.R) facilitates this. 

5. **Apply Correction**  
   Use the regression equation to adjust each monitor’s community deployment readings:
   - Corrected Value = (Raw Value - Intercept)/Slope

6. **Summarize Output**  
   Use [`summarize_pollution_times.R`](https://github.com/lewis-r-white/QuantAQ/blob/main/src/summarize_pollution_times.R) to summarize hourly and daily measurements. Save both hourly and daily summaries for corrected values.

**Output:**
- Corrected datasets saved to `/data/pm/final/`
- Summarized hourly and daily datasets saved to `/data/pm/summarized/`

### Gas Calibration: Golden Monitor-Based Regression

Gas-phase pollutants (CO, NO, NO₂, O₃) require a modified approach due to:
- Fewer colocated devices
- Inconsistent or unusual readings from certain monitors

**Steps:**

1. **Plot Correlations**  
   Create pairwise correlation plots and time series to assess monitor consistency during the colocation period.

2. **Select a Golden Monitor**  
   The monitor with the most stable measurements and high correlations with others (e.g., `MOD-00397`) is used as a reference.

3. **Run Regressions**  
   For each monitor and gas, regress observed values against the golden monitor’s values for the same timestamp. Within `pollutant_data_cleaning.Rmd`, the function `get_gas_regression_results` is created to facilitate this. 

4. **Apply Correction**  
   Use each monitor’s regression coefficients to adjust gas values for the community deployment period (post-colocation):
   - Corrected Gas = (Raw Gas - Intercept)/Slope

5. **Summarize Output**  
   Use [`summarize_pollution_times.R`](https://github.com/lewis-r-white/QuantAQ/blob/main/src/summarize_pollution_times.R) to summarize hourly and daily measurements. Save both hourly and daily summaries of corrected gas values.

**Output:**
- Corrected datasets saved to `/data/gas/final/`
- Summarized hourly and daily datasets saved to `/data/gas/summarized/`

**Note:**  
The gas correction step assigns NA slope/intercept values for monitors where the regression could not be calculated (e.g., too few points). 

All calibration scripts are implemented in `pollutant_data_prep.Rmd`. The code that goes through the calibration is immediately after loading and merging the raw datasets. This file sources `compare_fleet_regression.R` and `summarize_pollution_times.R`, and outputs analysis-ready data for subsequent visualization and modeling.


## 4. Monitor Metadata and Community Information

Key file: `monitor_community_info.csv` (Note: for privacy, no data is shared in this repository)

This dataset includes geospatial and contextual information for each QuantAQ monitor, including:

- Monitor serial number and type (MOD or MOD-PM)
- Community/village name and location
- Coordinates (latitude/longitude)
- Estimated population and household count

### Steps to Create `monitor_community_info.csv`:

1. **Extract Recent Coordinates**:  
   Coordinates were extracted from cloud data after March 2024, filtered for completeness and deduplicated per device. Some corrections were made manually to fix GPS errors in longitude.

2. **Integrate Population Data**:  
   Population and household counts were imported from KHRC's `modulair_communities.xlsx`. Community names were cleaned and standardized to match monitor locations (e.g., "Korawura Akura" → "Kurawura Akura").

3. **Assign Location Descriptions**:  
   Descriptive names and village-level locations were assigned to each monitor based on field records.

4. **Create a Master Reference Table**:  
   The final dataset combines monitor names, location coordinates, spatial metadata, and population information into a single CSV file:  
   `data/monitor_community_info.csv`

This reference file is used in analyses for spatial mapping, clustering, and aggregating pollution data by community.

# 5. Analysis 

## 5.1. Missing Data Analysis

**Key Files:**
- [`missing_data_analysis.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/analysis/data_completeness/missing_data_analysis.Rmd): Visualizes data completeness by season and geography, and quantifies gaps across monitors.
- [`cloud_vs_sd_completeness.R`](https://github.com/lewis-r-white/QuantAQ/blob/main/analysis/data_completeness/cloud_vs_sd_completeness.R): Compares SD card vs. cloud data availability, generates completeness heatmaps, and builds summary tables.
- [`gas_pm_completeness.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/analysis/data_completeness/gas_pm_completeness.Rmd): Examines MOD device pollutant-specific availability (PM1, PM2.5, CO, NO, NO2, O3) across weekly/monthly intervals.
  
- `data/missingness/sd_time_data_20230815-20250129.rds`: Pre-processed timestamp-only SD card data used to estimate completeness at the hourly and daily level.
- `data/pm/final/pm25corrected_20231024-20240816.rds`: Merged and corrected PM2.5 dataset used to assess recovery rates during Harmattan and non-Harmattan seasons.

**Description:**

This section quantifies and visualizes the availability of data collected by the monitors over time, with attention to completeness thresholds, seasonal variation, and differences between sources.
- **Completeness thresholds** are based on:
  - ≥45 minutes of data = 1 complete hour
  - ≥12 hours = 1 complete day
- **Visualizations**:
  - Weekly completeness heatmaps for SD, cloud, and merged data.
  - Spatial recovery maps of data completeness per monitor.
  - Harmattan vs. non-Harmattan plots for seasonal diagnostics.   
- **Summary tables**:
  - Earliest and latest dates with available data per source
  - Total hours and days of data recorded
  - Percent missing and recovered per monitor
- **Additional analysis** 
  - Stacked bar plots of cloud vs. SD contributions.
  - Ratio plots of Harmattan to non-Harmattan completeness.
  - Wilcoxon tests on paired completeness metrics.

These diagnostics highlight the need to merge cloud and SD card data.

## 5.2. Measurement Trends

This section evaluates pollution patterns across time, space, and environmental context using descriptive analyses, spatial mapping, and linear regression models.

**Key Files:**

- [`ghana_pm_report.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/analysis/trends/ghana_pm_report.Rmd)
  - Analyzes particulate matter (PM1, PM2.5, PM10) trends.
  - Includes:
    - Descriptive plots (e.g., time series, boxplots)
    - Spatial mapping of monitor readings
    - Comparisons of pollution during:
      - Cooking vs. non-cooking hours
      - Harmattan vs. non-Harmattan seasons
    - Linear regressions against fleet averages to correct and compare measurements
    - ANOVA and Tukey HSD tests to assess differences across times and locations

- [`ghana_gas_report.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/analysis/trends/ghana_gas_report.Rmd)
  - Examines gaseous pollutants (CO, NO, NO₂, O₃).
  - Includes:
    - Descriptive plots (e.g., time series, boxplots)
    - Spatial mapping of monitor readings
    - Comparisons of pollution during:
      - Cooking vs. non-cooking hours
      - Harmattan vs. non-Harmattan seasons
    - Linear regressions comparing each monitor to the golden monitor
    - Statistical tests to assess differences across times and locations

- [`create_wind_rose_plots.R`](https://github.com/lewis-r-white/QuantAQ/blob/main/analysis/trends/create_wind_rose_plots.R)
  - Creates wind rose plots to visualize wind direction and speed.
  - Compares Modulair device data to local weather station data.
  - Helps assess whether devices are reliably capturing wind information.

**Data Used:**
- Primarily summarized hourly and daily datasets (from Section 3 above). 

**Outputs:**
- See the [**plots**](https://github.com/lewis-r-white/QuantAQ/tree/main/plots) section of this repository for examples of visual outputs.



