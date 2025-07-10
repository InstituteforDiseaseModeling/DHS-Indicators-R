#' @export
read_recode <- function(sc, type, phase, cols, countries = NULL) {
  box::use(dplyr[...], sparklyr[...])
  query = paste0("SELECT _cc,_vv,", paste0(cols, collapse=","),  " FROM dhs.", phase, "_recode.", type)
  recode_tbl <- tbl(sc, sql(query))

  if (!is.null(countries)) {
    recode_tbl <- recode_tbl %>% filter(`_cc` %in% countries)
  }

  return(recode_tbl)
}

#' @export
read_indicator <- function(sc, chapter, countries = NULL) {
  box::use(dplyr[...], sparklyr[...])
  indicator_tbl <- tbl(sc, sql(paste0("SELECT * FROM dhs.indicator.chapter_", chapter)))

  if (!is.null(countries)) {
    indicator_tbl <- indicator_tbl %>% filter(`_cc` %in% countries)
  }

  return(indicator_tbl)
}

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

#' @export
read_indicator_metadata <- function(sc, chapter) {
  box::use(dplyr[...], sparklyr[...])

  metadata_tbl <- tbl(sc, sql(paste0("SELECT * FROM dhs.indicator.metadata")))
  metadata_tbl <- metadata_tbl %>% 
    filter(chapter == chapter) %>% 
    mutate(value_label = to_json(value_label))

  return(metadata_tbl)
}

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