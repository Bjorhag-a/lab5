#' @title Get municipalities
#' 
#' @description yuppy
#' 
#' 
#' @returns Will return a vector containing all municipalities that are a kommun 
#'   
#' @examples get_municipalities()
#' 
#' @import httr
#' @import jsonlite
#' 
#' @export


get_municipalities <- function(){
  res <- GET("http://api.kolada.se/v2/municipality")
  
  # return error code, if API does not return 200
  if (res$status != 200){
    return(m_id_res_status)
  } 
  
  data <- fromJSON(rawToChar(res$content))
  df <- rbind.data.frame((data[2]))
  
  # filter for kommun and leave out regions
  subset <- subset(df, values.type=="K")
  return (subset$values.title)
}
