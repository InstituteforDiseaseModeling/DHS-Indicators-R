#!/usr/bin/env Rscript

# ******************************************************************************
# Program: 			run_dhs_analysis.R
# Purpose: 			Download and process DHS data files from Azure blob storage
# Data outputs:		Analysis results for each country
# 
# Usage:            Rscript run_dhs_analysis.R --cores=N [--dry-run=TRUE/FALSE]
# Example:          Rscript run_dhs_analysis.R --cores=4 --dry-run=TRUE
# ******************************************************************************

# Load required libraries
suppressPackageStartupMessages({
  library(AzureStor)      # For Azure blob storage access
  library(AzureAuth)      # For Azure authentication
  library(tidyverse)      # For data manipulation
  library(parallel)       # For parallel processing
  library(optparse)       # For command line arguments
  library(here)          # For path handling
  library(fs)            # For file system operations
})

# Parse command line arguments
option_list <- list(
  make_option("--cores", type="integer", default=1, help="Number of cores for parallel processing"),
  make_option("--temp-dir", type="character", default="temp_dhs_data", help="Temporary directory for downloaded files"),
  make_option("--version", type="character", default="7", help="DHS version to process (e.g., '7')"),
  make_option("--tenant", type="character", help="Azure tenant ID (optional, will use Azure CLI credentials if not provided)", default="296b3838-4bd5-496c-bd4b-f456ea743b74"),
  make_option("--refresh", type="logical", default=FALSE, help="Force refresh of Azure token"),
  make_option("--dry-run", type="logical", default=TRUE, help="Only print commands without executing them"),
  make_option("--config", type="character", default="config/config.R", help="Path to the configuration file"),
  make_option("--country", type="character", default=NULL, help="Only process the specified country file prefix (e.g., 'NG')")
)

opt <- parse_args(OptionParser(option_list=option_list))

# Ensure opt$dry-run is logical
opt$`dry-run` <- as.logical(opt$`dry-run`)

# Countries supported

# Countries supported
countries <- read.csv("countries.csv", stringsAsFactors = FALSE)
# Filter to a specific country if --country is provided
if (!is.null(opt$country) && nzchar(opt$country)) {
  message(sprintf("Filtering to country: %s", opt$country))
  countries <- countries[countries$file_prefix == opt$country, ]
  if (nrow(countries) == 0) {
    stop(sprintf("No country with file_prefix '%s' found in countries.csv", opt$country))
  }
}

# Load configuration based on the provided parameter
config_file <- opt$config
source(config_file)

# Function to connect to Azure blob storage
connect_to_azure <- function() {
  message("Attempting to connect to Azure...")
  
  # Try to get current Azure CLI account info
  cli_account <- tryCatch({
    message("Checking Azure CLI login status...")
    system2("az", args = c("account", "show"), stdout = TRUE, stderr = TRUE)
    TRUE
  }, error = function(e) {
    message("Azure CLI not available or not logged in")
    FALSE
  })
  
  if (!cli_account) {
    message("Please log in to Azure CLI first using:\n",
            "az login\n",
            "Then run this script again.")
    stop("Azure CLI authentication required")
  }
  
  # Get the storage account token
  tryCatch({
    message("Getting Azure storage token...")
    token <- AzureAuth::get_azure_token(
      resource = "https://storage.azure.com",
      tenant = opt$tenant,
      app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",  # Azure CLI app ID
      use_cache = TRUE,
      auth_type = "device_code"
    )
    
    message("Creating storage endpoint...")
    endpoint <- storage_endpoint(
      AZURE_ENDPOINT,
      token = token,
      api_version = "2019-12-12"  # Explicitly set API version
    )
    
    message("Connecting to container...")
    cont <- storage_container(endpoint, AZURE_CONTAINER)
    
    # Test the connection
    message("Testing connection...")
    test_list <- list_blobs(cont)
    if (length(test_list) == 0) {
      message("Connected but no files found. Check container permissions.")
    } else {
      message("Successfully connected to Azure storage!")
    }
    
    return(cont)
    
  }, error = function(e) {
    message("\nDetailed error information:")
    message("Error message: ", e$message)
    
    # Check common issues
    if (grepl("Unauthorized", e$message)) {
      message("\nPossible solutions:")
      message("1. Ensure you're logged in to Azure CLI: Run 'az login'")
      message("2. Verify you have access to the storage account")
      message("3. Try running with explicit tenant ID: --tenant=YOUR_TENANT_ID")
    }
    
    if (grepl("NoAuthenticationInformation", e$message)) {
      message("\nAuthentication information missing:")
      message("1. Run 'az login' first")
      message("2. Verify your Azure CLI installation: 'az --version'")
      message("3. If problem persists, try running:")
      message("   az account get-access-token --resource https://storage.azure.com/")
    }
    
    stop("Azure authentication failed. See error details above.")
  })
}

# Function to extract file type from filename
get_file_type <- function(filename) {
  # Extract components using regex
  # Pattern matches file types like IR, MR, PR, etc. (case insensitive)
  if (grepl("IR", filename, fixed = TRUE, ignore.case = TRUE)) return("IR")
  if (grepl("MR", filename, fixed = TRUE, ignore.case = TRUE)) return("MR") 
  if (grepl("PR", filename, fixed = TRUE, ignore.case = TRUE)) return("PR")
  if (grepl("HR", filename, fixed = TRUE, ignore.case = TRUE)) return("HR")
  if (grepl("KR", filename, fixed = TRUE, ignore.case = TRUE)) return("KR")
  if (grepl("BR", filename, fixed = TRUE, ignore.case = TRUE)) return("BR")
  if (grepl("CR", filename, fixed = TRUE, ignore.case = TRUE)) return("CR")
  return(NA)
}

# Function to extract country code from filename
get_country_code <- function(filename) {
  # Extract components using regex - handle both full paths and basenames
  # Pattern matches country code (first 2 letters)
  basename <- basename(filename)
  country_code <- substr(basename, 1, 2)
  return(country_code)
}

# Function to check which input files are required for a chapter
check_input_files <- function(chapter_path) {
  # Default to requiring IR and MR files
  required_files <- list(IR = FALSE, MR = FALSE, PR = FALSE, HR = FALSE, KR = FALSE, BR = FALSE, CR = FALSE)
  
  # Check if there's a run_indicators.R file to analyze
  run_indicators_path <- file.path(chapter_path, "run_indicators.R")
 
  if (file.exists(run_indicators_path)){
    # Analyze run_indicators.R to determine required files
    # each chapter must implement run_indicators.R 
    # with the make_option that allows one or more following arguments
    # --ir|--mr|--pr|--hr|--kr|--br|--cr as well as --output-dir
    content <- readLines(run_indicators_path)
    
    # Look for command line arguments that indicate required files
    required_files$IR <- any(grepl("--ir=|IR.*dta|IR.*DTA", content))
    required_files$MR <- any(grepl("--mr=|MR.*dta|MR.*DTA", content))
    required_files$PR <- any(grepl("--pr=|PR.*dta|PR.*DTA", content))
    required_files$HR <- any(grepl("--hr=|HR.*dta|HR.*DTA", content))
    required_files$KR <- any(grepl("--kr=|KR.*dta|KR.*DTA", content))
    required_files$BR <- any(grepl("--br=|BR.*dta|BR.*DTA", content))
    required_files$CR <- any(grepl("--cr=|CR.*dta|CR.*DTA", content))
  }
  
  # Only return file types that are required
  return(required_files)
}

# Function to find all chapter folders
find_chapter_folders <- function() {
  # Look for folders starting with "Chap"
  chapter_dirs <- list.dirs(here(), recursive = FALSE)
  chapter_dirs <- chapter_dirs[grepl("^Chap", basename(chapter_dirs))]
  
  # Filter to only include folders that have run_indicators.R
  valid_chapters <- list()
  
  for (dir in chapter_dirs) {
    chapter_name <- basename(dir)
    # Check for run_indicators.R
    if (file.exists(file.path(dir, "run_indicators.R"))) {
      valid_chapters[[chapter_name]] <- dir
      next
    }
  }
  
  return(valid_chapters)
}

# Function to process a single country for a specific chapter
process_country_chapter <- function(country_code, chapter_info, files_by_type, container, temp_dir, dry_run = TRUE) {
  chapter_name <- chapter_info$name
  chapter_path <- chapter_info$path
  required_files <- chapter_info$required_files
  
  message(sprintf("Processing %s for chapter %s", country_code, chapter_name))
  
  # Create country directory if needed
  country_dir <- file.path(temp_dir, country_code)
  dir_create(country_dir)
  message(sprintf("folder %s created", country_dir))
  
  # Prepare the command parameters and download needed files
  cmd_params <- c("--output-dir=result")
  downloaded_files <- list()
  
  tryCatch({
    # For each required file type, find matching files and download
    for (file_type in names(required_files)) {
      if (required_files[[file_type]]) {
        # Find matching files for this country and file type
        matching_files <- files_by_type[[file_type]]
        if (!is.null(matching_files)) {
          matching_files <- matching_files[get_country_code(matching_files) == country_code]
          
          if (length(matching_files) > 0) {
            # Add version filter and check if any files match
            version_filtered_files <- matching_files[grepl(paste0(country_code, ".*", opt$version), basename(matching_files))]
            
            if (length(version_filtered_files) > 0) {
              # Process each matching file independently
              for (file_to_download in version_filtered_files) {
                tryCatch({
                  message(sprintf("Processing file: %s", file_to_download))
                  local_path <- file.path(country_dir, basename(file_to_download))
                  message(sprintf("Downloading %s to %s", file_to_download, local_path))
                  
                  if (dry_run==FALSE) {
                    # Download the file only if it doesn't exist already
                    if (!file.exists(local_path)) {
                      storage_download(container, file_to_download, local_path)
                    } else {
                      message(sprintf("File %s already exists. Skipping download.", local_path))
                    }
                  }
                  # Ensure file paths are full paths; if not, prepend the current working directory
                  if (!fs::is_absolute_path(local_path)) {
                    local_path <- fs::path_abs(local_path)
                  }
                  # Add to command parameters based on file type
                  cmd_param <- sprintf("--%s=%s", tolower(file_type), normalizePath(local_path))
                  message(sprintf("Adding command parameter: %s", cmd_param))
                  cmd_params <- c(cmd_params, cmd_param)
                  downloaded_files[[file_type]] <- c(downloaded_files[[file_type]], local_path)
                  
                }, error = function(e) {
                  message(sprintf("Failed to process file %s: %s", file_to_download, e$message))
                })
              }
              
              if (length(downloaded_files[[file_type]]) == 0) {
                message(sprintf("All %d matching files failed for %s", length(version_filtered_files), file_type))
                return(FALSE)
              }
            } else {
              message(sprintf("No %s files with version %s found for country %s", file_type, opt$version, country_code))
              # Don't immediately return FALSE, instead continue to other file types
            }
          } else {
            message(sprintf("No %s files with version %s found for country %s", file_type, opt$version, country_code))
            # If this file type is required but not found, we can't proceed
            if (required_files[[file_type]]) {
              message(sprintf("Required file type %s not found for country %s, skipping", file_type, country_code))
              return(FALSE)
            }
          }
        }
      }
    }
    
    # Check if we have enough files to proceed
    if (length(cmd_params) == 0) {
      message(sprintf("No data files found for country %s, skipping", country_code))
      return(FALSE)
    }
    
    # Determine which R script to run
    script_path <- NULL
    if (file.exists(file.path(chapter_path, "run_indicators.R"))) {
      script_path <- file.path(chapter_path, "run_indicators.R")
    }
    if (!is.null(script_path)) {
      # Build the command
      cmd <- paste("Rscript", script_path, paste(cmd_params, collapse = " "))
      
      # Execute or print the command
      if (!opt$`dry-run`) {
        message(sprintf("Executing: %s", cmd))
        system(cmd)
      } else {
        message(sprintf("Would execute: %s", cmd))
      }
    } else {
      message(sprintf("No run_indicators.R found for chapter %s", chapter_name))
    }
    
    # Clean up downloaded files if not in dry run mode
    if (opt$`dry-run`) {
      message("Cleaning up downloaded files")
      unlink(country_dir, recursive = TRUE)
    }
    
    return(TRUE)
    
  }, error = function(e) {
    message(sprintf("Error processing %s for chapter %s: %s", country_code, chapter_name, e$message))
    return(FALSE)
  })
}

# Main execution
main <- function() {
  # First check if required packages are installed
  required_packages <- c("AzureStor", "AzureAuth", "tidyverse", "parallel", "optparse", "here", "fs")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop("Missing required packages. Please install:\n",
         paste("  install.packages(c('", paste(missing_packages, collapse = "', '"), "'))", sep = ""))
  }
  
  message("Starting Azure DHS data analysis...")
  message("Current working directory: ", getwd())
  
  # Load all required packages
  suppressPackageStartupMessages({
    for (pkg in required_packages) {
      library(pkg, character.only = TRUE)
    }
  })
  
  # Connect to Azure
  container <- connect_to_azure()
  
  # Find all chapter folders
  message("Finding chapter folders with run_indicators.R...")
  chapter_folders <- find_chapter_folders()
  message(sprintf("Found %d valid chapter folders", length(chapter_folders)))
  
  # Create chapter info list with required files for each chapter
  chapter_info <- list()
  for (name in names(chapter_folders)) {
    path <- chapter_folders[[name]]
    required_files <- check_input_files(path)
    
    if (!is.null(required_files)) {
      message(sprintf("Chapter %s requires the following files:", name))
      for (file_type in names(required_files)) {
        if (required_files[[file_type]]) {
          message(sprintf("  %s: Required", file_type))
        }
      }
      chapter_info[[name]] <- list(
      name = name,
      path = path,
      required_files = required_files
      )
    }
  }
  
  # List files from blob storage
  message("Listing files in blob storage...")
  all_files <- list_blobs(container, prefix=AZURE_PATH)$name
  # Filter out the AZURE_PATH folder itself
  all_files <- all_files[all_files != AZURE_PATH]
  
  # Organize files by type (IR, MR, PR, etc.)
  files_by_type <- list()
  for (file_path in all_files) {
    file_type <- get_file_type(file_path)
    if (!is.na(file_type)) {
      if (is.null(files_by_type[[file_type]])) {
        files_by_type[[file_type]] <- character(0)
      }
      files_by_type[[file_type]] <- c(files_by_type[[file_type]], file_path)
    }
  }
  
  # Get all unique country codes
  country_codes <- unique(sapply(all_files, get_country_code))
  missing_prefixes <- setdiff(countries$file_prefix, country_codes)
  available_prefixes <- intersect(countries$file_prefix, country_codes)
  # Report missing ones
  if (length(missing_prefixes) > 0) {
    message("The following file prefixes are in config but not found in Azure storage.")
    print(countries[countries$file_prefix %in% missing_prefixes, c("file_prefix", "country_name")])
  } else {
    message("All configured file prefixes are present in Azure storage.")
  }
  
  # Create temp directory
  temp_dir <- opt$`temp-dir`
  dir_create(temp_dir)
  
  # For each chapter, process all countries
  results <- list()
  
  for (ch_name in names(chapter_info)) {
    ch_data <- chapter_info[[ch_name]]
    message(sprintf("Processing chapter %s", ch_name))
    
    # Check which file types this chapter needs
    required_types <- names(ch_data$required_files)[unlist(ch_data$required_files)]
    message(sprintf("Chapter %s requires file types: %s", ch_name, paste(required_types, collapse = ", ")))
    
    # Create a list of all country/chapter combinations
    tasks <- list()
    for (country in available_prefixes) {
      task_id <- paste(country, ch_name, sep = "_")
      tasks[[task_id]] <- list(
        country_code = country,
        chapter_info = ch_data
      )
    }
    
    # Process countries in parallel or sequentially
    if (opt$cores > 1 && !opt$`dry-run`) {
      cl <- makeCluster(opt$cores)
      on.exit(stopCluster(cl))
      
      ch_results <- parLapply(cl, tasks, function(task) {
        process_country_chapter(
          task$country_code, 
          task$chapter_info, 
          files_by_type, 
          container, 
          temp_dir, 
          opt$`dry-run`
        )
      })
    } else {
      ch_results <- lapply(tasks, function(task) {
        tryCatch({
          process_country_chapter(
            task$country_code, 
            task$chapter_info, 
            files_by_type, 
            container, 
            temp_dir, 
            opt$`dry-run`
          )
        }, error = function(e) {
          message(sprintf("Error processing country %s: %s", task$country_code, e$message))
          return(FALSE) # Return FALSE to indicate failure but continue processing
        })
      })
    }
    
    results <- c(results, ch_results)
  }
  
  # Clean up
  if (opt$`dry-run`) {
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Report results
  success_count <- sum(unlist(results))
  message(sprintf("Completed processing. Success: %d/%d", success_count, length(results)))
}

# Run main function
main()