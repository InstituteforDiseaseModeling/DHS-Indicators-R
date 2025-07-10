#' @export
get_config <- function () {
  return(list(
    chapter="7"
  ))
}

#' @export
load_data <- function(sc, phase = NULL, countries = NULL) {
  box::use(dplyr[...], sparklyr[...], etl/io)
  # TODO: support additional phases
  phase <- "7"

  ir_cols <- c(
    "caseid",
    "v000",
    "v001",
    "v002",
    "v003",
    "v004",
    "v005",
    "v007",
    "v008",
    "v013",
    "v024",
    "v025",
    "v106",
    "v190",
    "v201",
    "v213",
    "v215",
    "v217",
    "v222",
    "v225",
    "v301",
    "v302",
    "v302a",
    "v304_01",
    "v304_02",
    "v304_03",
    "v304_05",
    "v304_06",
    "v304_07",
    "v304_08",
    "v304_09",
    "v304_10",
    "v304_11",
    "v304_13",
    "v304_14",
    "v304_16",
    "v304_17",
    "v304_18",
    "v305_01",
    "v305_02",
    "v305_03",
    "v305_04",
    "v305_05",
    "v305_06",
    "v305_07",
    "v305_08",
    "v305_09",
    "v305_10",
    "v305_11",
    "v305_13",
    "v305_14",
    "v305_16",
    "v305_17",
    "v305_18",
    "v312",
    "v313",
    "v317",
    "v320",
    "v323",
    "v323a",
    "v326",
    "v362",
    "v375a",
    "v376",
    "v384a",
    "v384b",
    "v384c",
    "v384d",
    "v393a",
    "v394",
    "v395",
    "v3a08d",
    "v3a01",
    "v3a02",
    "v3a03",
    "v3a04",
    "v3a05",
    "v3a06",
    "v502",
    "v528",
    "v605",
    "v626a",
    "v632",
    "m6_1",
    "m10_1",
    "s309b",
    "s313",
    "NULL as s607c", # This is from MR3
    "NULL as s607d", # This is from GA3 and TZ3
    "v512",
    "v525",
    "v011",
    "v017",
    "v018",
    "v019",
    "v020",
    "v021",
    "v023",
    "v101",
    "v102",
    "vcal_1",
    "vcal_2",
    "vcal_3",
    "vcal_4",
    "vcal_5",
    "vcal_6",
    "vcal_7",
    "vcal_8",
    "vcal_9"
  )

  ir_tbl <- io$read_recode(sc, "ir", phase, ir_cols, countries)

  mr_cols <- c(
    "mcaseid",
    "mv000",
    "mv001",
    "mv002",
    "mv003",
    "mv004",
    "mv005",
    "mv301",
    "mv304_01",
    "mv304_02",
    "mv304_03",
    "mv304_04",
    "mv304_05",
    "mv304_06",
    "mv304_07",
    "mv304_08",
    "mv304_09",
    "mv304_10",
    "mv304_11",
    "mv304_12",
    "mv304_13",
    "mv304_14",
    "mv304_15",
    "mv304_16",
    "mv304_17",
    "mv304_18",
    "mv384a",
    "mv384b",
    "mv384c",
    "mv384d",
    "mv502"
  )

  mr_tbl <- io$read_recode(sc, "mr", phase, mr_cols, countries)
  return(list(IRdata=collect(ir_tbl), MRdata=collect(mr_tbl)))
}

#' @export
run <- function(data) {
  IRdata <- run_indicators(data$IRdata, NULL)
  MRdata <- run_indicators(NULL, data$MRdata)

  return(list(IRdata=IRdata, MRdata=MRdata))
}

#' @export
process <- function(data) {
  box::use(dplyr[...])

  IRdata <- select(data$IRdata, c(`_cc`, `_vv`, caseid, starts_with("fp_")))
  IRdata <- mutate(IRdata, `_pk` = caseid, `_recode_type` = "IR")
  MRdata <- select(data$MRdata, c(`_cc`, `_vv`, mcaseid, starts_with("fp_")))
  MRdata <- mutate(MRdata, `_pk` = mcaseid, `_recode_type` = "MR")

  return(bind_rows(list(IRdata, MRdata)))
}

run_indicators <- function(IRdata, MRdata) {
  box::use(Chap07_FP/FP_KNOW)
  box::use(Chap07_FP/FP_COMM)
  box::use(Chap07_FP/FP_USE)
  box::use(Chap07_FP/FP_Need)

  # Run analysis scripts
  result <- FP_KNOW$CREATE_FP_KNOW(IRdata=IRdata, MRdata=MRdata)
  message("Processed FP_KNOW...")
  result <- FP_COMM$CREATE_FP_COMM(IRdata=result$IRdata, MRdata=result$MRdata)
  message("Processed FP_COMM...")
  IRdata <- FP_USE$CREATE_FP_USE(IRdata=result$IRdata)
  message("Processed FP_USE...")
  IRdata <- FP_Need$CREATE_FP_NEED(IRdata=IRdata)
  message("Processed FP_Need...")

  if (!is.null(IRdata)) {
    return(IRdata)
  }
  if (!is.null(result$MRdata)) {
    return(result$MRdata)
  }
}