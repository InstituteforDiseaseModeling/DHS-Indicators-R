# ******************************************************************************
# Program:         run_indicators_spark.R
# Purpose:         Process and export microdata with all indicators (function version)
# Data outputs:    DTA files with all indicators at individual level
# ******************************************************************************
#' Run indicators processing for microdata (function version)
#' @param IRdata data.frame for IR data (or NULL)
#' @param MRdata data.frame for MR data (or NULL)
#' @return data.frame for either IRdata and MRdata (after processing)
#' @export
run_indicators <- function(IRdata = NULL, MRdata = NULL) {
  box::use(Chap07_FP/FP_KNOW)
  box::use(Chap07_FP/FP_COMM)
  box::use(Chap07_FP/FP_USE)
  box::use(Chap07_FP/FP_Need)

  chap <- "Chap07_FP"

  # Run IR analysis scripts
  result <- FP_KNOW$CREATE_FP_KNOW(IRdata=IRdata, MRdata=MRdata)
  message("Processed IRdata for FP_KNOW...")
  result <- FP_COMM$CREATE_FP_COMM(IRdata=result$IRdata, MRdata=result$MRdata)
  message("Processed IRdata for FP_COMM...")
  IRdata <- FP_USE$CREATE_FP_USE(IRdata=result$IRdata)
  message("Processed IRdata for FP_USE...")
  IRdata <- FP_Need$CREATE_FP_Need(IRdata=IRdata)
  message("Processed IRdata for FP_Need...")

  if (!is.null(IRdata)){
    message(paste("columns: ", ncol(IRdata)))
    return(IRdata)
    #all_cols <- colnames(IRdata)
    #return(IRdata[, c(fp_prefix, req_cols)])
  } else if (!is.null(MRdata)){
    message(paste("columns: ", ncol(MRdata)))
    return(MRdata)
    #all_cols <- colnames(MRdata)
    #return(MRdata[, c(fp_prefix, req_cols)])
  }
}
