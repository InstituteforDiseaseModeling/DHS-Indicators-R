#!/usr/bin/env Rscript

# ******************************************************************************
# Program: 			init_renv.R
# Purpose: 			Initialize and sync renv environment with project dependencies
# Date:				March 2025
#
# This script will:
# 1. Initialize renv if not already active
# 2. Sync all packages from renv.lock
# 3. Verify all required packages are properly installed
# 
# Usage: Rscript init_renv.R
# 
# After running this script successfully, other R scripts can be run directly
# as all dependencies will be properly set up.
# ******************************************************************************

cat("Initializing renv environment...\n\n")

# Test 1: Initialize/activate renv
cat("1. Initializing/activating renv:\n")
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
renv::load()

# Test 2: Show current status before sync
cat("\n2. Current renv status before sync:\n")
renv::status()

# Test 3: Restore/sync from lock file
cat("\n3. Syncing with renv.lock file:\n")
renv::restore(prompt = FALSE)

# Test 4: Show updated status
cat("\n4. Updated renv status after sync:\n")
renv::status()

# Test 5: List library paths
cat("\n5. Checking library paths:\n")
.libPaths()

# Test 6: Try loading key packages
packages_to_test <- c(
  "tidyverse",
  "tidyselect",
  "haven",
  "labelled",
  "expss",
  "xlsx",
  "here",
  "optparse",
  "DHS.rates"
)

cat("\n6. Testing package loading:\n")
for (pkg in packages_to_test) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat(sprintf("✓ Successfully loaded %s\n", pkg))
  }, error = function(e) {
    cat(sprintf("✗ Failed to load %s: %s\n", pkg, e$message))
  })
}

# Test 7: Print package versions and compare with lock file
cat("\n7. Installed package versions vs lock file:\n")
installed_versions <- installed.packages()[packages_to_test, "Version"]
lock_content <- renv::lockfile_read("renv.lock")

for (pkg in packages_to_test) {
  installed_ver <- installed_versions[pkg]
  lock_ver <- if (!is.null(lock_content$Packages[[pkg]])) {
    lock_content$Packages[[pkg]]$Version
  } else {
    "Not in lock file"
  }
  
  cat(sprintf("%s:\n  Installed: %s\n  Lock file: %s\n", 
              pkg, installed_ver, lock_ver))
}

cat("\nInitialization complete! The renv environment is now ready to use.\n")
cat("You can now run R scripts directly.\n") 