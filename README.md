# QuantAQ Air Quality Data Analysis and Dashboard

This repository contains scripts, datasets, and a Shiny dashboard related to air quality monitoring using QuantAQ devices across the Bono East region of Ghana.

## Project Structure

- `analysis/` – Analysis scripts and outputs (e.g., trends, completeness)
- `archive/` – Archived reports, scripts, and earlier datasets
- `air_pollution_dashboard/` – Shiny dashboard app:  
  [View Dashboard](https://cumc-columbia-air-pollution-dashboard.shinyapps.io/cumc-air-pollution-dashboard/)
- `data/` – Raw, processed, and summarized datasets
- `data_load_and_prep/` – Scripts for data ingestion and cleaning
- `plots/` – Figures used in reports and visualizations
- `src/` – Functions used in cleaning, merging, and analysis
- `QuantAQ.Rproj` – RStudio project file

The sections below describe the workflow used to **load, clean, and analyze** air quality data from QuantAQ **MODULAIR** and **MODULAIR-PM** devices.

---

## 1. Data Access and Loading Cloud + SD Card Data

### Key file: [`load_ghana_AQ_data.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/data_load_and_prep/load_ghana_AQ_data.Rmd)

This script walks through the process of loading and preparing data from:

- QuantAQ's **cloud-based API**
- **SD card exports** processed by the QuantAQ team

### Loading Cloud Data via API

To download data directly from the QuantAQ cloud:

#### Requirements

- A QuantAQ account with **developer access**
- A valid **API key** (generate under `Developer > API Keys` on the QuantAQ dashboard)
- Access to the specific devices you want to query

#### Workflow Steps

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

The script `load_ghana_AQ_data.Rmd` includes:

- Functions to extract `timestamp_iso`, pollutant concentrations, and metadata
- Automatic parsing of monitor names from file paths
- Standardization of column names to align with cloud data

This ensures cloud and SD card data can be merged consistently for downstream analysis.


## 2. Merging Cloud and SD Card Data (PM, Gas, and Weather)

### Key files: 
### - [`pollutant_data_cleaning.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/data_load_and_prep/pollutant_data_cleaning.Rmd)
### - [`weather_data_cleaning.Rmd`](https://github.com/lewis-r-white/QuantAQ/blob/main/data_load_and_prep/weather_data_cleaning.Rmd)

Once the cloud and SD card datasets are loaded, they are merged to create a complete time series per monitor. This ensures data continuity even during periods when cloud data is missing due to connectivity issues or device syncing delays.

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
  Applies the merging logic above across multiple pollutants, returning a structured list for downstream analysis.

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
   For each monitor, run a linear regression comparing its readings to the fleet average. Store the slope, intercept, RMSE, and MAE.

5. **Apply Correction**  
   Use the regression equation to adjust each monitor’s community deployment readings:
   - Corrected Value = (Raw Value - Intercept)/Slope

6. **Summarize Output**  
   Save both hourly and daily summaries for corrected values.

**Output:**
- Corrected datasets saved to `/data/pm/final/`
- Summarized hourly and daily datasets saved to `/data/pm/summarized/`

### Gas Calibration: Golden Monitor-Based Regression

Gas-phase pollutants (CO, NO, NO₂, O₃) require a modified approach due to:
- Fewer colocated devices
- Inconsistent or unusual readings from certain monitors

**Steps:**

1. **Select a Golden Monitor**  
   The monitor with the most stable measurements and high correlations with others (e.g., `MOD-00397`) is used as a reference.

2. **Plot Correlations**  
   Create pairwise correlation plots and time series to assess monitor consistency during the colocation period.

3. **Run Regressions**  
   For each monitor and gas, regress observed values against the golden monitor’s values for the same timestamp.

4. **Apply Correction**  
   Use each monitor’s regression coefficients to adjust gas values for the community deployment period (post-colocation):
   - Corrected Gas = (Raw Gas - Intercept)/Slope

6. **Summarize Output**  
   Save both hourly and daily summaries of corrected gas values.

**Output:**
- Corrected datasets saved to `/data/gas/final/`
- Summarized hourly and daily datasets saved to `/data/gas/summarized/`

**Note:**  
The gas correction step assigns NA slope/intercept values for monitors where the regression could not be calculated (e.g., too few points). 

All calibration scripts are implemented in `pollutant_data_prep.Rmd`. The code that goes through the calibration is immediately after loading and merging the raw datasets. This file sources `compare_fleet_regression.R` and `summarize_pollution_times.R`, and outputs analysis-ready data for subsequent visualization and modeling.
