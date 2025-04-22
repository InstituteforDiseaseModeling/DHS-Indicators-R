# Utilities for DHS Indicators

This directory contains utility functions used across different chapters of the DHS Indicators codebase.

## Micro Data Utilities

The `write_micro.R` file contains functions for writing and appending microdata across different chapters.

### Key Features

- Creates consistent microdata files with `_micro.dta` suffix
- Appends new variables when processing the same data file through multiple chapters
- Tracks which chapter added which variables

### Usage

When running analysis across multiple chapters, the micro data files can be used to collect all indicators in one place:

```r
# Example: Processing a dataset through multiple chapters

# First run Chapter 7 FP
Rscript Chap07_FP/run_indicators.R --ir=NGIR7BFL.dta --mr=NGMR7AFL.dta --output-dir=output

# Then run another chapter (e.g., Chapter 10) on the same data
# New variables will be appended to the existing microdata files
Rscript Chap10_XX/run_indicators.R --ir=NGIR7BFL.dta --mr=NGMR7AFL.dta --output-dir=output
```

### Available Functions

- `write_IR_micro(IRdata, input_filename, output_dir = NULL, chapter_tag = NULL)`
  - Writes or appends Women's Individual Recode (IR) data
  
- `write_MR_micro(MRdata, input_filename, output_dir = NULL, chapter_tag = NULL)`
  - Writes or appends Men's Individual Recode (MR) data
  
- `write_PR_micro(PRdata, input_filename, output_dir = NULL, chapter_tag = NULL)`
  - Writes or appends Person Recode (PR) data

Each function:
1. Checks if an output file already exists
2. If it exists, only appends new variables not already in the file
3. If it doesn't exist, creates a new file with all variables
4. Optionally adds a `chapter_source` column to track where variables came from

### Implementation in Chapter Scripts

To implement this in a chapter's microtables script:

```r
# Source the utility functions
source(here("utils/write_micro.R"))

# Process data as usual...

# Use the utility function to write/append data
write_IR_micro(IRdata, opt$ir, opt$output_dir, chapter_tag = "CHAPTER_TAG")
``` 