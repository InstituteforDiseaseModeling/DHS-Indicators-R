# ******************************************************************************
# Program:         run_indicators_spark.R
# Purpose:         Process and export microdata with all indicators (function version)
# Data outputs:    DTA files with all indicators at individual level
# ******************************************************************************


#' Run indicators processing for microdata (function version)
#' @param IRdata data.frame for IR data (or NULL)
#' @param MRdata data.frame for MR data (or NULL)
#' @return list with IRdata and MRdata (after processing)
run_indicators <- function(IRdata = NULL, MRdata = NULL) {

  chap <- "Chap07_FP"
  # Process and export IRdata if provided
  if (!is.null(IRdata)) {
    assign("IRdata", IRdata, envir = .GlobalEnv)
    message("Processing IRdata for micro export...")
    message(paste("columns: ", ncol(IRdata)))
     # Run IR analysis scripts
    source(here(paste0(chap,"/FP_KNOW.R")), local = .GlobalEnv)
    message("Processed IRdata for FP_KNOW...")
    source(here(paste0(chap,"/FP_USE.R")), local = .GlobalEnv)
    message("Processed IRdata for FP_USE...")
    source(here(paste0(chap,"/FP_Need.R")), local = .GlobalEnv)
    message("Processed IRdata for FP_Need...")
    source(here(paste0(chap,"/FP_COMM.R")), local = .GlobalEnv)
    message("Processed IRdata for FP_COMM...")
    IRdata <- get("IRdata", envir = .GlobalEnv)
    # message(paste("columns: ", ncol(IRdata)))
    all_cols <- colnames(IRdata)
    fp_prefix <- all_cols[grepl("fp_", all_cols)]
    # message(paste("output total: ", length(fp_prefix)))  
    return(IRdata[, c(fp_prefix, "v001", "v002", "v003")])
  }

  # Process and export MRdata if provided
  
  if (!is.null(MRdata)) {
    assign("MRdata", MRdata, envir = .GlobalEnv)
    message("Processing MRdata for micro export...")
    source(here(paste0(chap,"/FP_KNOW.R")), local = .GlobalEnv)
    message("Processed MRdata for FP_KNOW...")
    source(here(paste0(chap,"/FP_COMM.R")), local = .GlobalEnv)
    message("Processed MRdata for FP_COMM...")
    all_cols <- colnames(MRdata)
    fp_prefix <- all_cols[grepl("^fp_", all_cols)]
     return(MRdata[, c(fp_prefix, "mv001", "mv002", "mv003")])
  }
  
  message("Micro data export complete!")
  
}
