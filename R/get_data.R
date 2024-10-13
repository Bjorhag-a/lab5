
#' @title Download data from the Kolada API
#' 
#' @description
#' This function retrieves data for a specific KPI for a given municipality and
#' year from the Kolada API
#' 
#'
#' @param kpi The specific question/query ID in the API
#' @param municipality The municipality in the API
#' @param year The year in the API
#'
#' @returns a data frame of the specific arguments you have selected in the
#'   function and if no data is available for the given parameters, it returns
#'   `-1`.
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @export
#'
#' @examples 
#' # Fetch KPI data from the municipality  Helsingborg for the year 2019
#' get_data("N09890", "Helsingborg", "2019")



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

