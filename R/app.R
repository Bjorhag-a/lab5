
#' @title run_app 
#' 
#' @description yuppy
#' 
#' @param u ui
#' @param s server
#' 
#' @import shiny
#' @import ggplot2
#' @import magrittr
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
  
  d <- reactive({
    
    #if (is.data.frame(d())) {
    #  return(d())
    #} else {
    #  stop("No data for the selected inputs")
    #}
    
    get_data(input$kpi, input$m, input$year)}
    ) %>% 
    bindCache(input$kpi, input$m, input$year)
  
  
  # TODO check if a dataframe was returned or an error
  # print message to the user if error
  

  #---------------------------
  #COMMMMMMMMMENT FROM ALEXANDER
  #Have problem with the if statement because right now when i run the get_data function its a LIST 
  #So i dont know if that fucks things up because it works when its a "LIST" like we had it b efore but when i 
  #Say that it must be a dataframe the graph says that its not a dataframe and cant print it out.....
  #So wait for you to see if you have changed the get_data function xddddddddddd
  #--------------------------------------

  # TODO dont make to many repetitive API calls
  # cache results
  # maybe try memoise function

  output$plot <- renderPlot({
    validate(
      need(is.data.frame(d), "The inputs have no data")
    )
    
    
    ggplot(d(), aes(x = gender, y = value, fill=gender)) + 
      geom_bar(stat = "identity") +
      
      # TODO: print name of KPI --> Select one KPI group 
      labs(title = paste("Data for", input$m, "-", input$year, "-", input$kpi),
           x = "Gender", y = "Values") +
      theme_bw()
  }) %>% 
    bindCache(d())
}















