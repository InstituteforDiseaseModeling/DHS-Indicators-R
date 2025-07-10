#' @export
run <- function(sc, chapter, phase = NULL, countries = NULL) {
  box::use(etl/io, etl/metadata)

  data <- chapter$load_data(sc, phase, countries)
  indicators <- chapter$run(data)
  indicators <- chapter$process(indicators)

  metadata <- metadata$build_metadata(indicators)

  io$write_metadata(sc, chapter$get_config()$chapter, metadata)
  io$write_indicators(sc, chapter$get_config()$chapter, indicators)
}