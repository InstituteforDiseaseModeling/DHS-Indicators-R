#' Joins geographic data into an existing DHS dataframe
#'
#' @param data The existing DHS dataframe - the dataframe must contain a column DHSCLUST to join
#' @param ge_data The geographic data to join
#' @return A Spark DataFrame containing the resulting joined data
#' @export
join_ge_data <- function(data, ge_data, cluster_col) {
  box::use(dplyr[...], sparklyr[...], etl/io)

  data <- data %>%
    left_join(
      select(ge_data, c("DHSCLUST", "LATNUM", "LONGNUM", "ADM1NAME", "URBAN_RURA", "_cc", "_vv")),
      by = c("DHSCLUST", "_cc", "_vv")
    )

  return (data)
}