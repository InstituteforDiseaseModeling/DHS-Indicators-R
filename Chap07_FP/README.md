# Family Planning Indicator Generation: Two Approaches

This folder provides two main approaches for generating family planning indicator variables for DHS data:

- **1. Stata File Approach (Classic R, local files):**  
  Uses `run_indicators.R` to process Stata `.dta` files directly in R.
- **2. Databricks/Spark Approach (Cloud, scalable):**  
  Uses `chapter.R` (with Spark) to process data stored in Databricks tables.

---

## 1. Stata File Approach (`run_indicators.R`)

This script is designed for local or server-based R environments where you have access to the raw DHS Stata files (`.dta`). It reads, processes, and outputs family planning indicators and tables.

### **How it works:**
- **Inputs:**  
  - Individual Recode (IR) and/or Male Recode (MR) Stata files (e.g., `NGIR7BFL.dta`, `NGMR7AFL.dta`)
- **Processing:**  
  - Loads the required columns from recode file based on [required_col.csv](../required_col.csv).
  - Runs a series of scripts to generate family planning knowledge, use, need, and communication indicators.
  - Exports processed microdata and generates HTML reports.
- **Outputs:**  
  - Microdata files with indicator variables (DTA format)
  - HTML reports with summary tables

### **Usage:**
```sh
Rscript run_indicators.R --ir=FILENAME.dta --mr=FILENAME.dta [--output-dir=DIRECTORY] [--ge-dir=DIRECTORY]
```
- `--ir`: Path to IR Stata file (optional, but at least one of `--ir` or `--mr` is required)
- `--mr`: Path to MR Stata file (optional)
- `--output-dir`: Directory for output files (optional; defaults to a temp directory)
- `--ge-dir`: Directory for geospatial data (optional)

**Example:**
```sh
Rscript run_indicators.R --ir=NGIR7BFL.dta --mr=NGMR7AFL.dta --output-dir=output --ge-dir=geospatial_data_root
```

**Outputs will be found in** `output/Chap07_FP/` (or the specified directory), including logs, microdata, and HTML reports. 
- Currently run_indicators.R script only support either "ir" or "mr" option not both. 
- if GE file with matching pattern is found under <i>geospatial_data_root</i>, the result will be joining to DHSCLUST column (for example:
NGIR7BFL.dta will be matched to NGGE7BFL.dta) and the output will include columns "ADM1NAME", "URBAN_RURA", "LATNUM", "LONGNUM".

---

## 2. Databricks/Spark Approach (`chapter.R`)

This approach is designed for cloud-scale processing using Spark (e.g., on Databricks). It reads DHS recode data from Databricks tables (views), processes indicators in a distributed fashion, and is suitable for large-scale or multi-country analyses.

### **How it works:**
- **Inputs:**  
  - DHS data stored as Spark tables (not local `.dta` files)
- **Processing:**  
  - Loads required columns from Spark tables for IR and MR datasets.
  - Runs the same indicator generation logic as the Stata approach, but distributed via Spark.
  - Optionally joins geospatial data.
- **Outputs:**  
  - Data frames with indicator variables (can be exported to various formats or used for further analysis in Spark)
  - Can be extended to write results to Databricks tables, files, or dashboards.

### **Usage:**
This approach is typically used within a Databricks notebook or a Spark-enabled R session. Example workflow:

```r
# Connect to Spark
library(sparklyr)
sc <- spark_connect(method = "databricks")

# Source the chapter script
source("Chap07_FP/chapter.R")

# Load data from Spark tables
data <- load_data(sc, phase = "7", countries = c("NG"))

# Run indicator generation
results <- run(data)

# Optionally process and join geospatial data
final_data <- process(results)
```

---

## Summary

| Approach         | Script                | Input Type         | Output Type                                              | Environment         |
|------------------|----------------------|--------------------|----------------------------------------------------------|---------------------|
| Stata File       | `run_indicators.R`   | Stata `.dta` files | DTA (stata file format)                                  | Local/server R      |
| Databricks/Spark | `chapter.R`          | Spark tables       | DataFrames (can be persisted to csv or Databrick tables) | Databricks/Spark R  |

---

**Choose the approach that matches your data storage and analysis environment. Both methods generate harmonized family planning indicator variables using the same core logic.** 