## QuantAQ Ghana Air Quality Data Pipeline & Analysis

This repository contains the full data-processing and analysis pipeline for air quality monitoring using QuantAQ MODULAIR and MODULAIR-PM devices deployed across communities in Bono East, Ghana.

It includes workflows to:

-   Load cloud-based data from the QuantAQ API

-   Load SD card data processed by QuantAQ

-   Merge cloud and SD card sources into unified time series

-   Calibrate PM and gas measurements

-   Generate summarized hourly and daily datasets

-   Produce analysis outputs and visualizations

-   Support an interactive Shiny dashboard


## Repository Structure
QuantAQ_ghana/
│
├── README.md                     # High-level overview (this file)
├── docs/                         # Detailed documentation & pipeline descriptions
│   └── quantAQ_pipeline_overview.md
│
├── data/                         # Raw, merged, final & summarized datasets (not in GitHub)
│
├── data_load_and_prep/           # Main ETL workflows (RMarkdown)
│   ├── load_data_from_cloud_API.Rmd
│   ├── pm_data_prep.Rmd
│   ├── gas_data_prep.Rmd
│   ├── weather_data_prep.Rmd
│   ├── load_clean_GRIMM_data.Rmd
│   ├── FEM_calibration.Rmd
│   └── README.md
│
├── src/                          # Reusable functions for loading/merging/calibration
│   ├── load_pollution_datasets.R
│   ├── load_and_merge_pm_data.R
│   ├── merge_sd_data_gas.R
│   ├── build_gas_calibration.R
│   ├── compare_fleet_regression.R
│   ├── summarize_pollution_times.R
│   └── README.md
│
├── analysis/                     # Trend analysis, completeness, maps, plots
│   ├── data_completeness/
│   ├── trends/
│   └── README.md
│
├── air_pollution_dashboard/      # Shiny dashboard + code
│
├── plots/                        # Generated figures
│
└── archive/                      # Deprecated workflows, old scripts, legacy reference


## **Running the Data Preparation Pipeline — Order of Operations**

Most collaborators will want to run workflows in this order:

### **1. Load Data (Cloud data from API)**

data_load_and_prep/load_data_from_cloud_API.Rmd

-   Downloads QuantAQ data via API

-   Outputs a single combined cloud dataset

-   Includes code to clean SD card data that has been processed by QuantAQ team as well

-   Saves to: `data/all_measurements/cloud/`

### **2.** Prepare PM Data

data_load_and_prep/pm_data_prep.Rmd

-   Loads cloud + SD card PM data

-   Merges into unified minutely datasets

-   Performs PM colocation calibration (fleet-average regression)

-   Applies correction to community-period PM readings

-   Summarizes hourly & daily PM1, PM2.5, PM10

-   Saves corrected & summarized datasets

### **3.** Prepare Gas Data

data_load_and_prep/gas_data_prep.Rmd

-   Loads cloud + SD card gas data

-   Merges into unified datasets

-   Uses golden monitor regression for CO, NO, NO₂, O₃

-   Applies calibration to community-period gas readings

-   Summarizes hourly & daily gases

-   Saves corrected & summarized datasets

### **4.** Prepare Weather Data

data_load_and_prep/weather_data_prep.Rmd

-   Loads temperature, RH, wind speed, wind direction from cloud & SD

-   Merges and harmonizes variables

-   Summarizes hourly and daily weather

-   Saves merged & summarized datasets

### 5. Load & Calibrate GRIMM FEM for Validation

data_load_and_prep/load_clean_GRIMM_data.Rmd

data_load_and_prep/FEM_calibration.Rmd

data_load_and_prep/build_GRIMM_calibration_model.Rmd

## Analysis Workflows

analysis/data_completeness/ — Missingness, completeness heatmaps, SD vs cloud contributions

analysis/trends/ — PM and gas time series, seasonal comparisons, fleets, maps, wind roses

air_pollution_dashboard/ — Live dashboard built using summarized outputs

## Where to Find Detailed Documentation

All methodological details—including calibration theory, merging logic, completeness criteria, device metadata, and guidance on interpreting outputs—are located in:

-   docs/quantAQ_pipeline_overview.md

This document explains:

-   PM fleet-average calibration

-   Gas golden-monitor calibration

-   Colocation vs community periods

-   SD-card → cloud merging rules

-   Summarization thresholds (hour ≥45 min; day ≥12 hours)

-   File naming conventions

-   Example code snippets

### Contact & Collaboration

This repository was created by Lewis White and is maintained by the CHAP team.
