library(httr)
library(jsonlite)

get_municipalities <- function(){
  
}

get_data <- function(kpi, municipality, year){
  
  #TODO: check input
  
  # get municipality id
  m_id_res <- GET("http://api.kolada.se/v2/municipality", query=list(title=municipality))
  
  # return error code, if API does not return 200
  m_id_res_status <- m_id_res$status
  if (m_id_res_status != 200){
    return(m_id_res_status)
  } 
  
  m_id_data <- fromJSON(rawToChar(m_id_res$content))
  print(m_id_res_status)
  # use paste for the case of multiple ids for a municipality
  m_id <- paste(m_id_data$values$id, collapse = ",")
  print(m_id)
  
  
  
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
  return(df)
}

d<-get_data("N09890", "Helsingborg", 2019)
do.call(rbind.data.frame, d)
rbind.data.frame(d)
class(d)


kpi <- "N09890"
year <- "2019"
m_id <- c("0180")
paste(m_id, collapse = ",")
url <- paste("http://api.kolada.se/v2/data/kpi/",kpi,"/municipality/",paste(m_id, collapse = ","),"/year/", year,sep = "")
url
res <- GET(url)

data <- fromJSON(rawToChar(res$content))
values <- data$values$values

values







#TODO in shiny: don't fetch data too often, cache the data somehow










library(shiny)


ui <- fluidPage(
  
  page_sidebar(..., sidebar = NULL, title = NULL, fillable = TRUE, fillable_mobile = FALSE, 
               theme = bs_theme(), window_title = NA, lang = NULL)
  
  textInput(inputId = "curr", "Currency", "SEK"),
  numericInput(inputId = "p", 
               label = "Principal", 
               value = 100, min = 0),
  numericInput(inputId = "r", 
               label = "Interest Rate %", 
               value = 1, min = 0),
  numericInput(inputId = "t", 
               label = "Holding Period", 
               value = 1, min = 0),
  textOutput(outputId = "amt")
)

server <- function(input, output) {
  
  int <- reactive(input$pinput$rinput$t/100)
  output$amt <- renderText({
    paste("Amount at Maturity (Rounded to 2 decimal places):", 
          input$curr, as.character(round(input$p + int(),  2)))
  })
  
}

shinyApp(ui, server)


