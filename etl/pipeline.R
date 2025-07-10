#' Run the indicator generation pipeline
#'
#' @param chapter A "chapter" module containing the business logic for computing indicators for a given chapter
#' @param phase A string containing the DHS phase for which to generate indicators
#' @param countries An optional list of strings containing countries for which to generate indicators
#' @export
run <- function(sc, chapter, phase = NULL, countries = NULL) {
  box::use(etl/io, etl/metadata)

  message("Loading recode")
  data <- chapter$load_data(sc, phase, countries)

  message("Running indicators")
  indicators <- chapter$run(data)
  indicators <- chapter$process(indicators)

  message("Writing results")
  metadata <- metadata$build_metadata(indicators)

  io$write_metadata(sc, chapter$get_config()$chapter, metadata)
  io$write_indicators(sc, chapter$get_config()$chapter, indicators)
}