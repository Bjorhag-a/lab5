library(httr)
library(jsonlite)

get_data <- function(kpi, municipality, year){
  
  #TODO: check response code, if 200 or some error
  #TODO: check input
  #TODO: Stockholm for example has two IDs, Sometimes no data is available for certain years
  
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


