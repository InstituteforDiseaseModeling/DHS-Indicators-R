#' Build dataframe metadata
#'
#' Given a dataframe that contains variable and value labels, builds a dataframe
#' containing this extra metadata
#'
#' @param indicators A dataframe with labels
#' @return A dataframe containing the variable and value label metadata
#' @export
build_metadata <- function(data) {
  box::use(stats[...], tidyr[...], dplyr[...], jsonlite, labelled)

  variables <- data.frame(variable=colnames(data))

  var_labels <- labelled::get_variable_labels(data)
  var_labels <- pivot_longer(data.frame(Filter(function(x) !is.null(x), var_labels)), cols=everything(), names_to = "variable", values_to = "variable_label")

  val_labels <- labelled::val_labels(data)
  val_labels <- Filter(function(x) !is.null(x), val_labels)
  val_labels <- lapply(val_labels, function(x) {
    x <- lapply(x, as.character)
    x <- setNames(names(x), x)
    as.character(jsonlite::toJSON(as.list(x), auto_unbox=TRUE))
  })
  val_labels <- pivot_longer(data.frame(val_labels), cols=everything(), names_to = "variable", values_to = "value_label")

  metadata <- variables %>%
    left_join(var_labels, by = join_by(variable)) %>%
    left_join(val_labels, by = join_by(variable)) %>%
    filter(!is.na(variable_label) | !is.na(value_label))

  return(metadata)
}

#' Apply dataframe metadata
#'
#' @param data The dataframe to which to apply the metadata
#' @param metadata A dataframe containing the variable and label metadata
#' @return The original data dataframe with variable and value labels applied
#' @export
apply_metadata <- function(data, metadata) {
  box::use(stats[...], tidyr[...], dplyr[...], jsonlite, labelled)

  var_labels <- metadata %>%
    filter(variable %in% colnames(data)) %>%
    select(variable, variable_label)
  var_labels <- setNames(as.list(var_labels$variable_label), var_labels$variable)

  val_labels <- metadata %>%
    filter(variable %in% colnames(data)) %>%
    select(variable, value_label) %>%
    mutate(value_label = mapply(function (x) {
      x <- jsonlite::fromJSON(x)
      x <- setNames(names(x), x)
      lapply(x, as.numeric)
    }, value_label))
  val_labels <- setNames(lapply(val_labels$value_label, unlist), val_labels$variable)

  data <- labelled::set_variable_labels(data, .labels=var_labels)
  data <- labelled::set_value_labels(data, .labels=val_labels)

  return(data)
}