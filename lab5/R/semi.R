library(ggplot2)
city <- ggplot2city <- c("Stockholm", "Linköping", "Lund", "Mora")
year <- c("2017", "2018", "2019", "2020")
gender <- c("M", "F", "T")
val <- c(10, 6, 8)



library(shiny)
ui <- fluidPage(
  titlePanel("Kolada"),
  
  sidebarPanel(
    selectInput("m_id", "Select city", choices = c("Linköping", "Lund")),
    selectInput("kpi", "Select KPI", choices = c("hej", "då")),
    selectInput("year", "Select year", choices = c("1991", "1256")),
    
  ),
  mainPanel(
    plotOutput("barplot")
  )
)

server <- function(input, output){
  data <- reactive({
    new_data <- get_data(input$kpi, input$m_id, input$year)
    
    return(new_data)
  })
  
}

shinyApp(ui = ui, server = server)



data <- data.frame(gender, val)

ggplot(data, aes(x = gender, y = val )) + 
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_bw()



ui <- fluidPage(
  titlePanel("Kolada"),
  
  sidebarPanel(
    selectInput(inputId = "m_id", "Select city", choices = c("Linköping", "Lund")),
    selectInput(inputId = "kpi", "Select KPI", choices = c("hej", "då")),
    selectInput(inputId = "year", "Select year", choices = c("1991", "1256")),
    
  ),
  mainPanel(
    plotOutput("barplot")
  )
)

server <- function(input, output){
  
  data <- reactive({
    new_data <- get_data(input$kpi, input$m_id, input$year)
    
    return(new_data)
  })
  
  data <- new_data()
  
  output$barplot <- renderPlot({
    ggplot(data, aes(x = input$gender, y = input$value)) + 
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_bw()
  }, height = 400, width = 400)
  
}


shinyApp(ui = ui, server = server)