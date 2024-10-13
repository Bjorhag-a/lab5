#' @title Get KPIs for the specific KPI gruop
#' 
#' @description 
#' This function fetches all KPIs for the specific KPI group you have selected
#' and returns the data for the group ID
#' 
#' @param group_id id of the KPI group selected
#' 
#' @returns A data frame containing all KPIs for the specified KPI group
#'   
#' @examples 
#' # Get all KPIs for the KPI group GKPI127
#' get_kpi_group("GKPI127")
#' 
#' @import httr
#' @importFrom jsonlite fromJSON 
#' 
#' @export


get_kpi_group <- function(group_id){
  res <- GET("http://api.kolada.se/v2/kpi_groups")
  
  # return error code, if API does not return 200
  if (res$status != 200){
    return(res$status)
  } 
  
  data <- fromJSON(rawToChar(res$content))
  df <- rbind.data.frame((data$values))
  
  # filter for kommun and leave out regions
  return (df[df$id==group_id,])
}

