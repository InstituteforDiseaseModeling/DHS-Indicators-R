##
#' Find and Load Geospatial Shapefile for a Country and Version
#'
#' This function searches recursively in the given base path for a folder or file matching the pattern for a country's geospatial data (e.g., "ngge71").
#' If a matching directory is found, it looks for a .shp file inside and loads it as an sf object. If a matching .shp file is found directly, it loads it.
#'
#' @param country_code Character. The country code (e.g., "ng").
#' @param version Character. The version string (e.g., "71").
#' @param base_path Character. The base directory to search for geospatial files.
#'
#' @return An sf object containing the shapefile data if found, otherwise NULL.
#' @importFrom sf st_read
#' @examples
#' # shape <- find_ge_file("ng", "71", "/path/to/geodata")
find_ge_file <- function(country_code, version, base_path) {
  
  # Build the search pattern: e.g. "ngge71" and only .shp files
  pattern <- paste0(country_code, "GE", version, ".*\\.shp$")

  # Recursively look for .shp files whose names contain that pattern
  matches <- list.files(path        = base_path,
                        pattern     = pattern,
                        recursive   = TRUE,
                        full.names  = TRUE,
                        ignore.case = TRUE)
  
  # Return the first match as an sf object if a shapefile exists, else NULL
  if (length(matches) >= 1) {
    message("Found matching pattern '", pattern, "':")
    message(paste0(matches, collapse = ", "))
    shp_file <- matches[1]
    # Load the shapefile as an sf object
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("The 'sf' package is required but not installed.")
    }
    shape <- sf::st_read(shp_file, quiet = TRUE)
    return(shape)
  } else {
    message(paste0("Unable to find ", pattern))
    return(NULL)
  }
}


##
#' Join Survey Data with Geospatial Shapefile
#'
#' This function left-joins survey data to a geospatial sf object by cluster ID
#'
#' @param survey_data Data frame. The survey data to join (must include a cluster ID column).
#' @param ge_shape sf object. The geospatial shape data (must include a DHSCLUST column).
#' @param survey_cluster_id Character. The name of the cluster ID column in the survey data (default: "v001").
#'
#' @return A data frame with survey data, longitude, and latitude columns, and no sf geometry.
#' @importFrom dplyr left_join mutate
#' @importFrom sf st_coordinates st_drop_geometry
#' @examples
#' # merged <- join_ge_file(survey_data, ge_shape)
join_ge_file<- function(survey_data, ge_shape, survey_cluster_id="v001")
{
  
  # Left‐join survey → shapefile by cluster ID
  merged_sf <- survey_data %>%
    left_join(ge_shape %>% select (c("DHSCLUST", "LATNUM", "LONGNUM", "ADM1NAME", "URBAN_RURA")), by = setNames("DHSCLUST", survey_cluster_id))
  
  # Extract lon/lat and drop sf geometry
  merged <- merged_sf %>%
    # mutate(
    #   lon = st_coordinates(geometry)[,1],
    #   lat = st_coordinates(geometry)[,2]
    # ) %>%
    sf::st_drop_geometry()
  
  # list columns not supported
  list_cols <- names(Filter(is.list, merged))
  print(list_cols)
  merged %>% select(-all_of(list_cols))
}


##
#' Bind Survey Data with Geospatial Data (if available)
#'
#' This function attempts to find and join geospatial data to the survey data. If no geospatial data is found, it returns the survey data unchanged.
#'
#' @param survey_data Data frame. The survey data to join.
#' @param country_code Character. The country code (e.g., "ng").
#' @param version Character. The version string (e.g., "71").
#' @param base_path Character. The base directory to search for geospatial files.
#'
#' @return A data frame with geospatial columns if available, otherwise the original survey data.
#' @examples
#' # merged <- bind_geodata(survey_data, "ng", "71", "/path/to/geodata")
bind_geodata <- function(survey_data, country_code, version, base_ge__path) {
  ge_shape <- find_ge_file(country_code, version, base_ge__path)
  if (!is.null(ge_shape)) {
    return(join_ge_file(survey_data, ge_shape))
  } else {
    return(survey_data)
  }
}

