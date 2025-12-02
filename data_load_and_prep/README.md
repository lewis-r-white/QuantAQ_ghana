## `data_load_and_prep` — Data Loading & Preparation Workflows

This folder contains the main workflows used to build the QuantAQ Ghana air-quality dataset from raw cloud and SD-card sources.\

These RMarkdown files are the entry point for generating PM, gas, and weather datasets. Note: these workflows rely on functions defined in the `src/` directory. Each document (Rmd workflow or R script) should walk through the process in markdown text or code comments.

### **Order of Operations:** 

**1) Load Cloud Data**

`load_data_from_cloud_API.Rmd`: Downloads minutely monitor data directly from the QuantAQ API.

-   Requires API key

-   Produces unified cloud dataset

-   Saves to: `data/all_measurements/cloud/`

**2) Prepare PM Data**

`pm_data_prep.Rmd`

-   Loads cloud + SD card PM1, PM2.5, PM10

-   Merges cloud and SD card data

-   Calibrates using fleet-average colocation regressions

-   Summarizes hourly & daily.

-   Outputs saved to `data/pm/merged/`, `data/pm/final/`, `data/pm/summarized/` .

**3) Prepare Gas Data**

`gas_data_prep.Rmd`

-   Loads cloud + SD card CO, NO, NO₂, O₃

-   Merges cloud and SD card data

-   Applies golden-monitor calibration

-   Summarizes hourly & daily

-   Outputs saved to `data/gas/merged/`, `data/gas/final/`, `data/gas/summarized/`.

**4) Prepare Weather Data**

`weather_data_prep.Rmd`

-   Loads temperature, RH, wind speed, wind direction

-   Merges cloud + SD card

-   Summarizes hourly & daily for temperature and RH

-   Outputs saved to `data/weather/merged/` and `data/weather/summarized/`.

**5) GRIMM Federal Equivalent Method (FEM) Calibration** (Optional)

`load_clean_GRIMM_data.Rmd`

`FEM_calibration.Rmd`

`build_GRIMM_calibration_model.Rmd`

These workflows import and process GRIMM FEM data, perform colocation QC, and allow optional FEM-to-Modulair calibration.

### **Additional Utilities in This Folder**

`build_fleet_groups.R`

-   Helper used to define fleet subsets for PM calibration

`calibration.yml`

-   Configuration file controlling PM calibration parameters
