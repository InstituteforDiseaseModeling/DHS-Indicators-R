#!/usr/bin/env Rscript

# ******************************************************************************
# Program: 			run_indicators.R
# Purpose: 			Parameterized entry point for the Family Planning Chapter analysis
# Data outputs:		coded variables and table output on screen and in excel tables.
# 
# Usage:            Rscript run_indicators.R --ir=FILENAME.dta --mr=FILENAME.dta [--skip-events] [--skip-discont] [--output-dir=DIRECTORY]
# Example:          Rscript run_indicators.R --ir=NGIR7BFL.dta --mr=NGMR7AFL.dta --output-dir=output
# ******************************************************************************

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyselect)
  library(haven)
  library(labelled)
  library(expss)
  library(xlsx)
  library(here)
  library(optparse)
})

# Parse command line arguments
option_list <- list(
  make_option("--ir", type="character", help="IR data file name"),
  make_option("--mr", type="character", help="MR data file name"),
  make_option("--skip-events", action="store_true", default=TRUE, help="Skip events analysis"),
  make_option("--skip-discont", action="store_true", default=TRUE, help="Skip discontinuation analysis"),
  make_option("--output-dir", type="character", default=NULL, help="Output directory for micro data files")
)

opt <- parse_args(OptionParser(option_list=option_list))

# Set chapter path
chap <- "Chap07_FP"

# Create output directory if specified and doesn't exist
if (!is.null(opt$`output-dir`)) {
  output_path <- file.path(opt$`output-dir`, chap)
} else {
  output_path <- file.path(tempdir(), chap)
  message(paste0("No output directory specified. Using ", output_path))
}

if (!dir.exists(output_path)) {
  message("Creating output directory: ", output_path)
  dir.create(output_path, recursive = TRUE)
}

# Redirecting logs to a file in the output directory

# Define log file path
log_file <- file.path(
  output_path,
  paste0(
    if (!is.null(opt$ir)) basename(opt$ir) else "",
    "_",
    if (!is.null(opt$mr)) paste0(basename(opt$mr), ".log") else ".log"
  )
)

# Open a connection to the log file
log_con <- file(log_file, open = "wt")

# Ensure all warnings and info messages are logged
if (!is.null(log_con)) {
    message("Redirecting output and messages to log file: ", log_file)
    options(warn = 1)  # Print warnings as they occur
    options(error = function(e) {
        message("Error: ", conditionMessage(e))
        sink(type = "output")
        sink(type = "message")
        close(log_con)
        stop(e)
    })
} else {
    warning("Log connection is NULL. Output and messages will not be redirected to a log file.")
}

sink(log_con, type = "output")
sink(log_con, type = "message")

# Ensure logs are flushed to the file
on.exit({
  sink(type = "output")
  sink(type = "message")
  close(log_con)
}, add = TRUE)

# Validate inputs
if (!is.null(opt$ir) || !is.null(opt$mr)) {
  if (!is.null(opt$ir)) {
    message("Processing IR file: ", opt$ir)
    IRdata <- read_dta(opt$ir)
    # Run IR analysis scripts
    source(here(paste0(chap,"/FP_KNOW.R")))
    source(here(paste0(chap,"/FP_USE.R")))
    source(here(paste0(chap,"/FP_NEED.R")))
    source(here(paste0(chap,"/FP_COMM.R")))
    source(here(paste0(chap,"/FP_microtables.R")))

  # Optional analyses
  if (!opt$`skip-events`) {
    message("Running events analysis...")
    source(here(paste0(chap,"/FP_EVENTS.R")))
  }

  if (!opt$`skip-discont`) {
    message("Running discontinuation analysis...")
    source(here(paste0(chap,"/FP_DISCONT.R")))
  }
  }
  if (!is.null(opt$mr)) {
    message("Processing MR file: ", opt$mr)
    MRdata <- read_dta(opt$mr)
    # Run MR analysis scripts
    source(here(paste0(chap,"/FP_KNOW.R")))
    source(here(paste0(chap,"/FP_COMM.R")))
    source(here(paste0(chap,"/FP_microtables.R")))
  }
} else {
  stop("Either IR or MR data file must be specified using --ir=FILENAME.dta or --mr=FILENAME.dta")
}

message("Analysis complete!")