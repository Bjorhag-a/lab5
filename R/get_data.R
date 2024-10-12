#' @title Get data 
#' 
#' @description yuppy
#' 
#' 
#' @returns Will return a data frame with fetched data from API for given parameters
#'   
#' @examples get_data()
#' 
#' @import httr
#' @import jsonlite
#' 
#' @export


get_data <- function(kpi, municipality, year){
  
  stopifnot(is.character(kpi), is.character(municipality), is.character(year))
  
  # get municipality id
  m_id_res <- GET("http://api.kolada.se/v2/municipality", query=list(title=municipality))
  
  # return error code, if API does not return 200
  m_id_res_status <- m_id_res$status
  if (m_id_res_status != 200){
    return(m_id_res_status)
  } 
  
  m_id_data <- fromJSON(rawToChar(m_id_res$content))
  # use paste for the case of multiple ids for a municipality
  m_id <- paste(m_id_data$values$id, collapse = ",")
  
  
  
  # get requested data
  res <- GET(paste("http://api.kolada.se/v2/data/kpi/",kpi,"/municipality/",m_id,"/year/", year,sep = ""))
  
  # return error code, if API does not return 200
  res_status <- m_id_res$status
  if (m_id_res_status != 200){
    return(res_status)
  } 
  data <- fromJSON(rawToChar(res$content))
  values <- data$values$values
  
  # return -1 if no data is available for the url parameters
  if (is.null(values)) {
    return (-1)
  }
  
  # convert list to dataframe
  df <- rbind.data.frame(values)
  # return subset with important columns
  return(df[c("gender", "value")])
}

d<-get_data("N09890", "Helsingborg", "2019")
d
data.frame(gender=c("K", "M", "T"),value=c(10.272117, 6.348855, 8.248716))










