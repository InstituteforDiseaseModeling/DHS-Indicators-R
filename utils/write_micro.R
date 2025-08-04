# ******************************************************************************
# Program: 			  write_micro.R
# Purpose: 		    Utility functions for writing microdata files with appending capability
# Data outputs:		DTA files with all indicators at individual level for different chapters
# Note: 				  Used by various chapters to maintain consistent micro data output
# ******************************************************************************

source(here("utils/geospatial.R"))

#' Get output filename for micro data
#' 
#' @param input_filename Original data filename 
#' @param suffix Optional suffix to append on filename
#' @return Full path to output file with _micro suffix
get_output_filename <- function(input_filename, suffix = NULL) {
  # Extract base filename without extension
  base_name <- tools::file_path_sans_ext(basename(input_filename))
  country_code <- substr(base_name, 1, 2)
  ext <- tools::file_ext(basename(input_filename))
  # Add _micro suffix if provided
  if (!is.null(suffix)){
    new_name <- paste0(base_name, suffix, ".", ext)
  } else{
    new_name <- paste0(base_name, ".", ext)
  }
  return(file.path(country_code, new_name))
}

#' Write IR micro data with appending capability
#' 
#' @param data IR data frame with indicators
#' @param input_filename Original IR data filename for naming output
#' @param ge_dir Directory where geospatial data lives
#' @param output_dir Optional output directory
#' @param chapter_tag Tag to identify which chapter generated these variables
#' @param dta_version Stata version to use for output file (default: 14)
#' @return Path to the output file
write_dta_micro <- function(data, input_filename, ge_dir, output_dir = NULL, chapter_tag = NULL, dta_version = 14) {
  message("Processing data for micro export...")

  # Construct the output path
  if (is.null(output_dir) || is.null(chapter_tag)) {
    stop("Both output_dir and chapter_tag must be provided.")
  }
  
  
  output_file <- get_output_filename(input_filename, "_var")
  output_path <- file.path(output_dir, chapter_tag, output_file)

  # Extract country_code and version from the input filename
  base_name <- tools::file_path_sans_ext(basename(input_filename))
  country_code <- substr(base_name, 1, 2)
  # Version is the two characters at positions 3 and 4
  version <- substr(base_name, 5, 6)

  # Bind geospatial data if available
  if (!is.null(ge_dir)){
    data <- bind_geodata(data, country_code, version, ge_dir)
  }
  # Check if DHSCLUST column exists (from spatial join)
  has_spatial <- all(c("LATNUM", "LONGNUM") %in% names(data))

  # Ensure the directory exists
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  # Write the data to the specified path
  message("Creating new output file: ", output_path)
  message("exporting data with ", ncol(data), " variables and ", nrow(data), " observations")
  haven::write_dta(data, output_path, version = dta_version)
  message("Successfully exported data with ", ncol(data), " variables and ", nrow(data), " observations")
  message(paste0("Geospatial data available: ", has_spatial))
  return(list(has_spatial=has_spatial, data=data))
}
