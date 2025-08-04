# Utilities for DHS Indicators

This directory contains utility functions used across different chapters of the DHS Indicators codebase.

## Micro Data Utilities

The `write_micro.R` file contains functions for writing microdata across different chapters.

### Key Features

- Creates consistent microdata files with `_micro.dta` suffix
- Overwrites existing files instead of appending
- Saves files in a structured path `{output_dir}/{chapter_tag}/{output_file}`

### Implementation in Chapter Scripts

To implement this in a chapter's microtables script:

```r
# Source the utility functions
source(here("utils/write_micro.R"))

# Process data as usual...

# Use the utility function to write data
write_IR_micro(IRdata, opt$ir, opt$output_dir, chapter_tag = "CHAPTER_TAG")
```