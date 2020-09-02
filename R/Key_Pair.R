#' Key_Pair
#'
#' @description Takes a dataframe and converts it into a json key value pair currently only returns as strings
#' #'
#' @param df is expected to be a dataframe extracted from an SF object
#'
#' @return should return a dataframe of key value pairs
#' @export
#'
#' @examples
#' library(sf)
#' nc <- st_read( system.file("shape/nc.shp", package="sf"))
#'
#' attri <- boundary %>% dplyr::mutate_all(~ gsub('"', "'", .)) %>%
#' as.data.frame() %>%  dplyr::select(-geometry) %>%  key_Pair()

key_Pair <- function(df){
  name <- names(df)

  for(i in 1:NCOL(df)){ # can I vectorise this for loop

    df[,i] <- sprintf('"%s":"%s"', name[i], df[,i])

  }
  return(df)
}
