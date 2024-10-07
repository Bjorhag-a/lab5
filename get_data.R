library(httr)
library(jsonlite)

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





library(thenerdyclient)

ui <- fluidPage(
  
  # Application title
  titlePanel("Find your Destiny!"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      h3(textInput(inputId = "begin",label = "Type here")),
      actionButton(inputId = "Find",label = "Find the location")
    ),
    
    # Show the map and location coordinates
    mainPanel(
      tags$style("#Location {font-size:25px;
               color:darkblue;
                 display:block; }"),     
      div(style="text-align:center;
        box-shadow: 10px 10px 5px #888888;
          width:350px;
          height:350px;
          padding-top:100px;
          position:relative;",
          textOutput(outputId = "Location")),
      div(style="text-align:center;
          width:1000px;
          height:1000px;
          padding-top:100px;
          position:relative;",
          leafletOutput(outputId = "mymap"))
    )
  )
)

# Define server logic required to plot the location in the map
server <- function(input, output) {
  
  
  reactivedata = reactive({
    return(latlong(input$begin))
  })
  
  
  observeEvent(input$Find,{
    
    
    
    x1=reactivedata()
    
    lat <- as.numeric(unname(x1[[1]]))
    lng <- as.numeric(unname(x1[[2]]))
    
    o1<-paste("Latitude:",lat,sep="")
    o2<-paste("Longitude:",lng,sep="")
    
    output$Location <- renderText(paste(c(o1,o2)))
    
    icon.fa <- makeAwesomeIcon(
      icon = "flag", markerColor = "red",
      library = "fa",
      iconColor = "black"
    )
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(
          "OpenStreetMap",
          # give the layer a name
          group = "OpenStreetMap"
        ) %>%
        addAwesomeMarkers(
          lat = lat,
          lng = lng,
          label = "Starting point",
          icon = icon.fa
        )
      
      
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


ui <- fluidPage(
  titlePanel("View quotes filtered by character and movie in the Lord of the Rings universe"),
  sidebarLayout(
    sidebarPanel(
      textInput("character", "Specify a character:", value="Frodo Baggins"),
      textInput("movie", "Specify a movie:", value="Return")
    ),
    mainPanel(tableOutput("filtered_quotes"))
  )
)

server <- function(input, output) {
  output$filtered_quotes <- renderTable({
    thenerdyclient::get_quote_data(input$character, input$movie)
  })
}

# Create and run the Shiny app
shinyApp(ui, server)
