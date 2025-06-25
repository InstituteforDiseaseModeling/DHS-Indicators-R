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

if (!is.null(opt$ir) && !is.null(opt$mr)) {
  log_file_name <- paste0(basename(opt$ir), "_", basename(opt$mr), ".log") 
} else if (!is.null(opt$ir)) {
  log_file_name <- paste0(basename(opt$ir), ".log")
} else if (!is.null(opt$mr)) {
  log_file_name <- paste0(basename(opt$mr), ".log")
} else {
  log_file_name <- "no_input_error.log"
}

log_file <- file.path(
  output_path,
  log_file_name
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
  
  source(here(paste0(chap,"/FP_KNOW.R")))
  source(here(paste0(chap,"/FP_USE.R")))
  source(here(paste0(chap,"/FP_Need.R")))
  source(here(paste0(chap,"/FP_COMM.R")))
  
  # Ensure all required columns exist in IRdata and MRdata
  req_cols <- read.csv(here( "required_col.csv"), stringsAsFactors = FALSE)
  if (!is.null(opt$ir)) {
    message("Processing IR file: ", opt$ir)
    IRdata <- read_dta(opt$ir)
    ir_cols <- req_cols$col_name[req_cols$survey_type == "ir"]
    for (col in ir_cols) {
      if (!col %in% colnames(IRdata)) {
        IRdata[[col]] <- NA
      }
    }
    IRdata <- IRdata[, ir_cols, drop = FALSE]
    # Run IR analysis scripts
    result <- CREATE_FP_KNOW(IRdata=IRdata, MRdata=NULL)
    message("Processed IRdata for FP_KNOW...")
    result <- CREATE_FP_COMM(IRdata=result$IRdata, MRdata=NULL)
    message("Processed IRdata for FP_COMM...")
    IRdata <- CREATE_FP_USE(IRdata=result$IRdata)
    message("Processed IRdata for FP_USE...")
    IRdata <- CREATE_FP_NEED(IRdata=IRdata)
    message("Processed IRdata for FP_Need...")
    
    source(here(paste0(chap,"/FP_microtables.R")))
    write_micro_variables(IRdata=IRdata, 
                          MRdata=NULL, 
                          source_filename_ir=opt$ir,
                          source_filename_mr=NULL,
                          output_dir=opt$`output-dir`)

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
    mr_cols <- req_cols$col_name[req_cols$survey_type == "mr"]
    for (col in mr_cols) {
      if (!col %in% colnames(MRdata)) {
        MRdata[[col]] <- NA
      }
    }
    MRdata <- MRdata[, mr_cols, drop = FALSE]
    # Run MR analysis scripts
    result <- CREATE_FP_KNOW(IRdata=NULL, MRdata=MRdata)
    message("Processed IRdata for FP_KNOW...")
    result <- CREATE_FP_COMM(IRdata=NULL, MRdata=result$MRdata)
    source(here(paste0(chap,"/FP_microtables.R")))
    write_micro_variables(IRdata=NULL, 
                          MRdata=result$MRdata, 
                          source_filename_ir=NULL,
                          source_filename_mr=opt$mr,
                          output_dir=opt$`output-dir`)
  }
} else {
  stop("Either IR or MR data file must be specified using --ir=FILENAME.dta or --mr=FILENAME.dta")
}

message("Analysis complete!")