get_data <- function(kpi, municipality, year){
  #TODO: check input
  #TODO: Stockholm for example has two IDs
  
  # get municipality id
  m_id_res <- GET("http://api.kolada.se/v2/municipality", query=list(title=municipality))
  m_id_data <- fromJSON(rawToChar(m_id_res$content))
  m_id <- m_id_data$values$id
  print(m_id)
  
  # get requested data
  res <- GET(paste("http://api.kolada.se/v2/data/kpi/",kpi,"/municipality/",m_id,"/year/", year,sep = ""))
  data <- fromJSON(rawToChar(res$content))
  values <- data$values$values
  
  
  return(values)
}

d<-get_data("N09890", "Helsingborg", 2019)
d