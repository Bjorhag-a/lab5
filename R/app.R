#' @title run_app 
#' 
#' @description yuppy
#' 
#' @param u ui
#' @param s server
#' 
#' @import shiny
#' @import ggplot2
#' 
#' @export
#' 

run_app <- function(u, s){
  shinyApp(ui = u, server = s)
}


ui <- fluidPage(
  titlePanel("Kolada"),
  sidebarLayout(
    sidebarPanel(
      selectInput("m", "Select municipality", choices = NULL),
      selectInput("kpi", "Select KPI", choices = c("N09890", "N09891")),
      selectInput("year", "Select year", choices = c(2018, 2019))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session){
  
  observe({
    updateSelectInput(session, "m", choices = get_municipalities())
  })
  
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















