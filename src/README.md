`src/` — Core Functions Used in the Data Pipeline

This folder contains reusable R functions that support the full QuantAQ Ghana data-processing workflow.

These functions are sourced by the .Rmd workflows in data_load_and_prep/ and should not be run standalone.

### **Main Function Categories**

#### **1) Data Loading Utilities**

Scripts that load PM, gas, and weather data from cloud or SD-card sources.

They standardize:

-   column names

-   datetime parsing

-   monitor IDs

-   Used in: `pm_data_prep.Rmd`, `gas_data_prep.Rmd`, `weather_data_prep.Rmd`

#### **2) Cloud + SD Merging Functions**

PM and gas measurements come from two sources.

These functions:

-   join cloud and SD data

-   resolve duplicates

-   apply source-priority rules

-   split data into colocation vs community periods

Used in: all pollutant prep workflows.

#### **3) Calibration Functions**

Two calibration approaches are supported:

-   PM fleet-average regression (colocation-based)

-   Gas golden-monitor regression

Functions in this group fit calibration models and apply correction formulas to community deployment data.

Used in: `pm_data_prep.Rmd`, `gas_data_prep.Rmd`.

#### **4) Time Summarization Functions**

General-purpose tools for computing:

-   hourly averaged data

-   daily averaged data

With built-in completeness checks:

-   valid hour = ≥45 minutes of data

-   valid day = ≥18 valid hours

-   Used across PM, gas, and weather workflows.

**Note:** These scripts are not designed to be run individually; they work as helper functions for .Rmd pipelines. All functions should return tidy data frames.
