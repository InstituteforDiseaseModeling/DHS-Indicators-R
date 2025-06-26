# ******************************************************************************
# Program:         run_indicators_spark.R
# Purpose:         Process and export microdata with all indicators (function version)
# Data outputs:    DTA files with all indicators at individual level
# ******************************************************************************
#' Run indicators processing for microdata (function version)
#' @param IRdata data.frame for IR data (or NULL)
#' @param MRdata data.frame for MR data (or NULL)
#' @return data.frame for either IRdata and MRdata (after processing)
run_indicators <- function(IRdata = NULL, MRdata = NULL) {

  chap <- "Chap07_FP"
  # source(here(paste0(chap,"/FP_KNOW.R")))
  # source(here(paste0(chap,"/FP_USE.R")))
  # source(here(paste0(chap,"/FP_Need.R")))
  # source(here(paste0(chap,"/FP_COMM.R")))
  # req_cols <- read.csv(here("required_col.csv"), stringsAsFactors = FALSE)
  req_cols <- read.csv("required_col.csv", stringsAsFactors = FALSE)
  ir_cols <- req_cols$col_name[req_cols$survey_type == "ir" & req_cols$chapter=="7"]
  mr_cols <- req_cols$col_name[req_cols$survey_type == "mr" & req_cols$chapter=="7"]
  
  if (!is.null(IRdata)){
    for (col in ir_cols) {
      if (!col %in% colnames(IRdata)) {
        IRdata[[col]] <- NA
      }
    }
  }
  if (!is.null(MRdata)){
    for (col in ir_cols) {
      if (!col %in% colnames(MRdata)) {
        MRdata[[col]] <- NA
      }
    }
  }
  
  # Run IR analysis scripts
  result <- CREATE_FP_KNOW(IRdata=IRdata, MRdata=MRdata)
  message("Processed IRdata for FP_KNOW...")
  result <- CREATE_FP_COMM(IRdata=result$IRdata, MRdata=result$MRdata)
  message("Processed IRdata for FP_COMM...")
  IRdata <- CREATE_FP_USE(IRdata=result$IRdata)
  message("Processed IRdata for FP_USE...")
  IRdata <- CREATE_FP_Need(IRdata=IRdata)
  message("Processed IRdata for FP_Need...")

  fp_prefix <- all_cols[grepl("fp_", all_cols)]
  
  # for spark operations, result must be table format
  if (!is.null(IRdata)){
    message(paste("columns: ", ncol(IRdata)))
    all_cols <- colnames(IRdata)
    return(IRdata[, c(fp_prefix, req_cols)])
  } else if (!is.null(MRdata)){
    message(paste("columns: ", ncol(MRdata)))
    all_cols <- colnames(MRdata)
    return(MRdata[, c(fp_prefix, req_cols)])
  }
  
}
