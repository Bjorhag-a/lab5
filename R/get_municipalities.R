#' @title Get all municipalities from the Kolada API
#' 
#' @description This function fetches all the municipalities from the Kolada API
#'   and filter out the regions in the API
#' 
#' @returns Return a vector containing all municipalities that are categorized
#'   as a kommun and extract the regions
#'   
#' @examples 
#' # Get a list of all municipalities (kommuner) from the Kolada API
#' get_municipalities()
#' 
#' @import httr
#' @importFrom jsonlite fromJSON 
#' 
#' @export


get_municipalities <- function(){
  res <- GET("http://api.kolada.se/v2/municipality")
  
  # return error code, if API does not return 200
  if (res$status != 200){
    return(res$status)
  } 
  
  data <- fromJSON(rawToChar(res$content))
  df <- rbind.data.frame((data[2]))
  
  # filter for kommun and leave out regions
  subset <- subset(df, values.type=="K")
  return (subset$values.title)
}


