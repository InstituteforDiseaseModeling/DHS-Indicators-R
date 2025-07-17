# Generating indicators for multiple countries at microdata level

## How to Start with renv
To set up the R environment using the provided `renv.lock` file, follow these steps:
1. Open your R console or RStudio.
2. Run the following commands:
   ```R
   install.packages("renv")
   renv::restore()
   ```
   This will install all the required packages as specified in the lock file.

## Implementation of Generic R-Indicators Across Countries
The script `run_dhs_analysis.R` is designed to implement generic R-indicators across multiple countries. It uses the `countries.csv` file to specify the input datasets and configurations for each country. To run the analysis:
1. Ensure `countries.csv` is updated with the correct file paths and configurations for each country.
2. Use `{config/config.R}` as template to add your azure blob path that contains teh raw data.
3. Make sure you do `az login` before you run the script and have access to the azure blob path.
4. Execute the script using:
   ```R
   Rscript run_dhs_analysis.R --dry-run=FALSE --config={your_azure_blob_config}
   ```
   This will process the data for all countries listed in `countries.csv`.

## Utilities
The `utils` folder contains utility scripts to support the analysis. For example, `write_micro.R` provides functions to export microdata to `.dta` format. The simplified `write_dta_micro` function:
- Saves the data to a specified path `{output_dir}/{chapter_tag}/{output_file}` while retain the basename of `{input_filename}`.
- Overwrites any existing file without appending.
- Ensures the directory structure exists before saving.

## Parameterized Scripts for Chapters
Each chapter should replace its `!main.R` and `{}_tables.R` scripts with parameterized scripts like `run_indicators.R` and `{}_microtables.R`. These scripts take input data and produce the required indicators and export microdata outputs. For example, in `Chap07_FP`:
- `run_indicators.R` processes the input data and generates indicators.
- `FP_microtables.R` outputs all results to `.dta` format at the micro level.
The original R scripts should be kept as is but may need handling of missing columns and data to account for difference in country surveys.

## Custom Indicators
Anyone who wishes to provide custom indicators should follow the same approach as stated above and make a new folder with meaningful names and add it to the execution list in `run_dhs_analysis.R`.

## Future Work
Future enhancements include merging the microdata with the DHS geospatial dataset to obtain longitude and latitude information. This should be implemented in the `run_dhs_analysis.R` script as a global approach, as all DHS data can be joined to the geospatial data using the same location ID. Refer to the DHS workbook for accurate details on the geospatial dataset structure and joining process.

# Data Storage and access
The microlevel data will be stored in IDM's databrick instances with strict policy controlled access and made available for modeling need



