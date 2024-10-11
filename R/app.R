#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Kolada"),
  sidebarLayout(
    sidebarPanel(
      #TODO: fetch municipalities automatically from https://api.kolada.se/v2/municipality
      # --> write new function for that
      selectInput("m", "Select municipality", choices = c("Helsingborg", "Stockholm", "Lund")),
      selectInput("kpi", "Select KPI", choices = c("N09890", "N09891")),
      selectInput("year", "Select year", choices = c(2018, 2019))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output){
  
  d <- reactive({get_data(input$kpi, input$m, input$year)})
  
  # TODO check if a dataframe was returned or an error
  # print message to the user if error
  
  # TODO dont make to many repetitive API calls
  # cache results
  # maybe try memoise function
  
  output$plot <- renderPlot({
    ggplot(d(), aes(x = gender, y = value, fill=gender)) + 
      geom_bar(stat = "identity") +
      
      # TODO: print name of KPI --> Select one KPI group 
      labs(title = paste("Data for", input$m, "-", input$year, "-", input$kpi),
           x = "Gender", y = "Values") +
      theme_bw()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)












