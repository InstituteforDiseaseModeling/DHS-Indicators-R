# ******************************************************************************
# Program: 			  write_micro.R
# Purpose: 		    Utility functions for writing microdata files with appending capability
# Data outputs:		DTA files with all indicators at individual level for different chapters
# Note: 				  Used by various chapters to maintain consistent micro data output
# ******************************************************************************

#' Get output filename for micro data
#' 
#' @param input_filename Original data filename 
#' @param output_dir Optional output directory
#' @return Full path to output file with _micro suffix
get_output_filename <- function(input_filename, output_dir = NULL) {
  # Extract base filename without extension
  base_name <- tools::file_path_sans_ext(basename(input_filename))
  # Add _micro suffix
  new_name <- paste0(toupper(base_name), "_MICRO.DTA")
  # Add output directory if provided
  if (!is.null(output_dir)) {
    new_name <- file.path(output_dir, new_name)
  }
  return(new_name)
}

#' Write IR micro data with appending capability
#' 
#' @param IRdata IR data frame with indicators
#' @param input_filename Original IR data filename for naming output
#' @param output_dir Optional output directory
#' @param chapter_tag Tag to identify which chapter generated these variables
#' @param dta_version Stata version to use for output file (default: 14)
#' @return Path to the output file
write_dta_micro <- function(IRdata, input_filename, output_dir = NULL, chapter_tag = NULL, dta_version = 14) {
  message("Processing IRdata for micro export...")

  # Construct the output path
  if (is.null(output_dir) || is.null(chapter_tag)) {
    stop("Both output_dir and chapter_tag must be provided.")
  }
  output_file <- get_output_filename(input_filename)
  output_path <- file.path(output_dir, chapter_tag, output_file)

  # Ensure the directory exists
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  # Write the data to the specified path
  message("Creating new output file: ", output_path)
  haven::write_dta(IRdata, output_path, version = dta_version)
  message("Successfully exported IRdata with ", ncol(IRdata), " variables and ", nrow(IRdata), " observations")

  return(output_path)
}
