#' @export
build_metadata <- function(chapter, indicators) {
  box::use(dplyr[...], jsonlite, labelled)

  variables <- data.frame(variable=colnames(indicators))

  var_labels <- labelled::get_variable_labels(indicators)
  var_labels <- pivot_longer(data.frame(Filter(function(x) !is.null(x), var_labels)), cols=everything(), names_to = "variable", values_to = "variable_label")

  val_labels <- labelled::val_labels(indicators)
  val_labels <- Filter(function(x) !is.null(x), val_labels)
  val_labels <- lapply(val_labels, function(x) {
    x <- lapply(x, as.character)
    x <- setNames(names(x), x)
    as.character(jsonlite::toJSON(as.list(x), auto_unbox=TRUE))
  })
  val_labels <- pivot_longer(data.frame(val_labels), cols=everything(), names_to = "variable", values_to = "value_label")

  metadata <- left_join(variables, var_labels, by = join_by(variable))
  metadata <- left_join(metadata, val_labels, by = join_by(variable))

  metadata <- filter(metadata, !is.na(variable_label) | !is.na(value_label))
  return(metadata)
}