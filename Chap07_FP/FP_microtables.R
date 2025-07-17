# ******************************************************************************
# Program: 			  FP_microtables.R
# Purpose: 		    Process and export microdata with all indicators 
# Data outputs:		DTA files with all indicators at individual level
# Note: 				  Uses utility functions from write_micro.R
# ******************************************************************************
#

# Source the utility functions
library(here)
source(here("utils/write_micro.R"))

write_micro_variables <- function(IRdata, MRdata, source_filename_ir, source_filename_mr, output_dir, ge_dir=NULL) {
  # Process and export IRdata if it exists
  
  box::use(haven[...], labelled[...], sjlabelled[...], expss[...])
  if (!is.null(IRdata)) {
    message("Processing IRdata for micro export...")
    
    # Apply weights for consistency (same as in FP_tables.R)
    IRdata <- IRdata %>%
      mutate(wt = v005/1000000)
    
    # Create standard population groups as done in FP_tables.R
    # This helps maintain consistency between tables and micro data
    
    # dummy var for all women
    IRdata <- IRdata %>% mutate(fp_all = case_when(
      v007>0  ~ "all"),
      fp_all = set_label(fp_all, label = "all women"))
    
    # only married women
    IRdata <- IRdata %>% mutate(fp_married = case_when(
      v502==1  ~ "married"),
      fp_married = set_label(fp_married, label = "currently married women"))
    
    # sexually active unmarried women (SAUW); sexually active if had sex in last 30 days
    IRdata <- IRdata %>% mutate(fp_sauw = case_when(
      v502!=1 & v528<=30 ~ "SAUW"),
      fp_sauw = set_label(fp_sauw, label = "sexually active, unmarried women"))
    
    # Use the utility function to write/append IR data
    result <- write_dta_micro(IRdata, source_filename_ir, ge_dir, output_dir, chapter_tag = "Chap07_FP")
    message("IR data export complete!") 
    return (result)
  }
  
  # Process and export MRdata if it exists
  if (!is.null(MRdata)) {
    message("Processing MRdata for micro export...")
    
    # Apply weights (same pattern as IRdata)
    MRdata <- MRdata %>%
      mutate(wt = mv005/1000000)
    
    # Create any MR-specific population groups if needed
    # This follows similar patterns as in IRdata but with MR variables
    
    # Filter for men in union/married if needed
    MRdata <- MRdata %>% mutate(fp_married = case_when(
      mv502==1  ~ "married"),
      fp_married = set_label(fp_married, label = "currently married men"))
    
    # Use the utility function to write/append MR data
    result <- write_dta_micro(MRdata, source_filename_mr, ge_dir, output_dir, chapter_tag = "Chap07_FP")
    message("MR data export complete!") 
    return (result)
  }
}
