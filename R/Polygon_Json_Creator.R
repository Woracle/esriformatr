#' Polygon_Json_Creator
#'
#' @description Takes an SF object with polygon data and coverts it to a ESRI format json string
#'
#' @param boundary is expected to be a SF object with boundary data
#' @param geometry is expected to be a string with the field name containing the geometry data
#' @param wkid a text string indicating the well know id to be used.
#'
#' @return returns a json string
#'
#' @export
#'
#' @examples
#'
#' library(sf)
#' nc <- st_read( system.file("shape/nc.shp", package="sf"))
#'
#' json <- esriformatr::Polygon_Json_Creator(nc)

Polygon_Json_Creator <- function(boundary, geometry = "geometry", wkid = "4326") {
  if (!"sf" %in% class(boundary)) {
    stop("boundary file not a simple features class")
  }
  if (!geometry %in% names(boundary)) {
    stop(paste("No column called", geometry, "in dataframe"))
  }

  # Collect data from sf object and
  # convert information into a set of key value pairs
  attri <- boundary %>% dplyr::mutate_all(~ gsub('"', "'", .)) %>%
    as.data.frame() %>%
    dplyr::select(-geometry) %>%
    key_Pair() %>%
    tidyr::unite(col = "attri", sep = ",") %>%
    c() %>% unlist

  # extract geometry field as a nested list
  # Convert bottom level of list into a dataframe
  polygon <- purrr::modify_depth(.x = boundary$geometry,
                          .depth =  3,
                          .f = as.data.frame) %>%
    # Combine columns of the dataframe into a string of cordinates
    purrr::modify_depth(
      .x = .,
      .depth =  3,
      .f = tidyr::unite,
      col = "coords",
      V1,V2,
      sep = ","
    ) %>%
    # Collapse the strings into json style string
    purrr::modify_depth(
      .x = .,
      .depth =  3,
      .f = ~ sprintf("[%s]", .$coords) %>%
        paste(collapse =  ",") %>%
        sprintf("[%s]", .)
    ) %>%

    # Unlist object
    purrr::modify_depth(
      .x = .,
      .depth = 1,
      .f =  unlist
    ) %>%
    # Collapse into a string of coordinates
    purrr::map(.x = .,
        .f = ~ paste(.x, collapse =  ","))

  # Insert attributes string and polygon string into esri json format
  # collapse seperate json objects into a single string
  json <- sprintf(
    '{"geometry":{"rings":[%s],"spatialReference":{"wkid":%s}},"attributes":{%s}}',
    polygon,
    wkid,
    attri
  ) %>% paste(collapse = ",")

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
