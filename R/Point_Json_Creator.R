#' Point_Json_Creator
#'
#' @description Takes an SF object with points data and coverts it to a ESRI format json string
#'
#' @param points is expected to be a SF object with boundary data
#' @param geometry is expected to be a string with the field name containing the geometry data
#' @param wkid a text string indicating the well know id to be used.
#'
#' @return a json string
#' @export
#'
#' @examples
#'
Point_Json_Creator <- function(points, geometry = "geometry", wkid = "4326") {
  if (!"sf" %in% class(points)) {
    stop("points file not a simple features class")
  }
  if (!geometry %in% names(boundary)) {
    stop(paste("No column called", geometry, "in dataframe"))
  }

  # Collect data from sf object and
  # convert information into a set of key value pairs
  attri <- points %>% dplyr::mutate_all(~ gsub('"', "'", .)) %>%
    as.data.frame() %>%
    dplyr::select(-geometry) %>%
    key_Pair() %>%
    tidyr::unite(col = "attri", sep = ",") %>%
    c() %>% unlist

  # Extract coordinates from sf object
  coords <- sf::st_coordinates(points)

  # Insert attributes string and polygon string into esri json format
  # collapse seperate json objects into a single string
  json <- sprintf(
    '{"geometry":{"x":%s,"y":%s,"spatialReference":{"wkid":%s}},"attributes":{%s}}',
    coords[,1],
    coords[,2],
    wkid,
    attri
  )%>% paste(collapse = ",")

  # This section converts string to json format
  # cleans out some artifacts of R strings
  # the uses prettify to test json is valid
  # then uses minify to minimise the object size
  json <- jsonlite::toJSON(json)
  json <- gsub("\\\\", "", json)
  json <- sprintf('[%s]',  substr(json, 3, nchar(json) - 2))
  json <- jsonlite::prettify(json)
  json <- jsonlite::minify(json)

  return(json)

}
