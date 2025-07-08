#' @export
read_recode <- function(sc, type, phase, cols, countries) {
  box::use(dplyr[...], sparklyr[...])
  query = paste0("SELECT _cc,_vv,", paste0(cols, collapse=","),  " FROM dhs.", phase, "_recode.", type)
  recode_tbl <- tbl(sc, sql(query))

  if (!is.null(countries)) {
    recode_tbl <- recode_tbl %>% filter(`_cc` %in% countries)
  }

  return(recode_tbl)
}

#' @export
write_metadata <- function(sc, chapter, metadata) {
  box::use(dplyr[...], sparklyr[...], DBI)

  metadata <- mutate(metadata, chapter = chapter$get_config()$chapter)
  tbl_metadata <- copy_to(sc, metadata, "_new_metadata", overwrite = TRUE)
  tbl_metadata <- mutate(tbl_metadata, value_label = parse_json(value_label))

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
      replaceWhere = paste0("chapter = '", chapter$get_config()$chapter ,"'")
    )
  )
}

#' @export
write_indicators <- function(sc, chapter, indicators) {
  box::use(dplyr[...], sparklyr[...])
  tbl_indicators <- copy_to(sc, indicators, "_indicators", overwrite = TRUE)
  countries <- unique(indicators$`_cc`)

  spark_write_table(
    tbl_indicators,
    name = paste0("dhs.indicator.chapter_", chapter$get_config()$chapter),
    mode = "overwrite",
    options = list(
      mergeSchema = "true",
      replaceWhere = paste0("_cc IN ('", paste0(countries, collapse="','", "')"))
    )
  )
}