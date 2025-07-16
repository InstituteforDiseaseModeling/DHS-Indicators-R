#' Reads recode data into a Spark DataFrame
#'
#' @param sc The SparkContext to use for Spark operations
#' @param type A string containing the type of recode data to read (e.g. ir, mr)
#' @param phase A string containing the DHS phase from which to read
#' @param cols A list of strings containing the columns to read
#' @param countries An optional list of strings containing the countries to read
#' @param versions An optional list of strings containing the versions to read
#' @return A Spark DataFrame containing the resulting recode data
#' @export
read_recode <- function(sc, type, phase, cols, countries = NULL, versions = NULL) {
  box::use(dplyr[...], sparklyr[...])
  query = paste0("SELECT _cc,_vv,", paste0(cols, collapse=","),  " FROM dhs.", phase, "_recode.", type)
  recode_tbl <- tbl(sc, sql(query))

  if (!is.null(countries)) {
    recode_tbl <- recode_tbl %>% filter(`_cc` %in% countries)
  }

  if (!is.null(versions)) {
    recode_tbl <- recode_tbl %>% filter(`_vv` %in% versions)
  }

  return(recode_tbl)
}

#' Reads indicator data into a Spark DataFrame
#'
#' @param sc The SparkContext to use for Spark operations
#' @param chapter A string containing the DHS chapter from which to read
#' @param countries An optional list of strings containing the countries to read
#' @return A Spark DataFrame containing the resulting indicator data
#' @export
read_indicator <- function(sc, chapter, countries = NULL) {
  box::use(dplyr[...], sparklyr[...])
  indicator_tbl <- tbl(sc, sql(paste0("SELECT * FROM dhs.indicator.chapter_", chapter)))

  if (!is.null(countries)) {
    indicator_tbl <- indicator_tbl %>% filter(`_cc` %in% countries)
  }

  return(indicator_tbl)
}

#' Reads geographic data into a Spark DataFrame
#'
#' @param sc The SparkContext to use for Spark operations
#' @param phase A string containing the DHS phase from which to read
#' @param countries An optional list of strings containing the countries to read
#' @param versions An optional list of strings containing the versions to read
#' @return A Spark DataFrame containing the resulting geographic data
#' @export
read_ge <- function(sc, phase, countries = NULL, versions = NULL) {
  box::use(dplyr[...], sparklyr[...])
  query = paste0("SELECT * FROM dhs.", phase, "_gis.ge")
  ge_tbl <- tbl(sc, sql(query))

  if (!is.null(countries)) {
    ge_tbl <- ge_tbl %>% filter(`_cc` %in% countries)
  }

  if (!is.null(versions)) {
    ge_tbl <- ge_tbl %>% filter(`_vv` %in% versions)
  }

  return(ge_tbl)
}

#' Reads recode metadata 
#'
#' This function requires both a country and version to be specified as the metadata
#' can vary across surveys even within the same DHS phase and country.
#'
#' @param sc The SparkContext to use for Spark operations
#' @param type A string containing the type of recode data to read (e.g. ir, mr)
#' @param country A string containing the country for which to read metadata
#' @param version A string containing the version for which to read metadata
#' @return A Spark DataFrame containing the resulting recode metadata
#' @export
read_recode_metadata <- function(sc, type, country, version) {
  box::use(dplyr[...], sparklyr[...])
  phase <- substr(version, 1, 1)

  metadata_tbl <- tbl(sc, sql(paste0("SELECT* FROM dhs.", phase, "_recode.metadata")))
  metadata_tbl <- metadata_tbl %>% 
    filter(type == type, `_cc` == country, `_vv` == version) %>% 
    mutate(value_label = to_json(value_label))

  return(metadata_tbl)
}

#' Reads indicator metadata 
#'
#' @param sc The SparkContext to use for Spark operations
#' @param chapter A string containing the DHS chapter for which to read metadata
#' @return A Spark DataFrame containing the resulting indicator metadata
#' @export
read_indicator_metadata <- function(sc, chapter) {
  box::use(dplyr[...], sparklyr[...])

  metadata_tbl <- tbl(sc, sql(paste0("SELECT * FROM dhs.indicator.metadata")))
  metadata_tbl <- metadata_tbl %>% 
    filter(chapter == chapter) %>% 
    mutate(value_label = to_json(value_label))

  return(metadata_tbl)
}

#' Write indicator metadata 
#'
#' @param sc The SparkContext to use for Spark operations
#' @param chapter A string containing the DHS chapter for which to write metadata
#' @param metadata A dataframe containing the metadata
#' @export
write_metadata <- function(sc, chapter, metadata) {
  box::use(dplyr[...], sparklyr[...], DBI)

  tbl_metadata <- copy_to(sc, metadata, "_new_metadata", overwrite = TRUE)
  tbl_metadata <- tbl_metadata %>% mutate(chapter = chapter, value_label = parse_json(value_label))

  DBI::dbGetQuery(sc, paste0(
    "CREATE TABLE IF NOT EXISTS dhs.indicator.metadata (",
    "chapter STRING, ",
    "variable STRING, ",
    "variable_label STRING, ",
    "value_label VARIANT",
    ")"
  ))

  spark_write_table(
    tbl_metadata,
    name = "dhs.indicator.metadata",
    mode = "overwrite",
    options = list(
      replaceWhere = paste0("chapter = '", chapter ,"'")
    )
  )
}

#' Write indicator data 
#'
#' @param sc The SparkContext to use for Spark operations
#' @param chapter A string containing the DHS chapter for which to write data
#' @param indicators A dataframe containing the indicator data
#' @export
write_indicators <- function(sc, chapter, indicators) {
  box::use(dplyr[...], sparklyr[...], labelled)

  indicators <- labelled::remove_labels(indicators)

  tbl_indicators <- copy_to(sc, indicators, "_new_indicators", overwrite = TRUE, memory = FALSE, repartition = 10L)
  target <- paste0("dhs.indicator.chapter_", chapter)

  DBI::dbGetQuery(sc, paste0(
    "CREATE TABLE IF NOT EXISTS ", target, " (",
    "_pk STRING, ",
    "_recode_type STRING, ",
    "_cc STRING, ",
    "_vv STRING",
    ")"
  ))

  DBI::dbGetQuery(sc, paste0(
    "MERGE WITH SCHEMA EVOLUTION INTO ", target, " ",
    "USING _new_indicators ",
    "ON _new_indicators._pk = ", target, "._pk ",
    "AND _new_indicators._recode_type = ", target, "._recode_type ",
    "AND _new_indicators._cc = ", target, "._cc ",
    "AND _new_indicators._vv = ", target, "._vv ",
    "WHEN MATCHED THEN UPDATE SET * ",
    "WHEN NOT MATCHED THEN INSERT *"
  ))
}