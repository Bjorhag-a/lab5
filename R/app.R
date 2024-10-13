#' @title Run the Kolada KPI with Shiny app
#' 
#' @description 
#' This function runs the Shiny app that allows us to display the data from 
#' the Kolada KPI. The app provides an interface so you can select municipality, KPI and year and 
#' then creates a bar plot divided by gender.  
#' 
#' @param u A shiny UI object that selects the interface of the app 
#' @param s A shiny server app that selects the server output of the app.
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
      selectInput("kpi", "Select KPI", choices = NULL),
      selectInput("year", "Select year", choices = c(
        "2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015",
        "2015",
        "2016",
        "2017",
        "2018", 
        "2019"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session){
  # get kpis for later reference for 
  kpi_group <- get_kpi_group("GKPI127")
  kpi_members <- rbind.data.frame(kpi_group$members)

  observe({
    updateSelectInput(session, "m", choices = get_municipalities())
    updateSelectInput(session, "kpi", choices = kpi_members$member_id)
  })
  
  d <- reactive({
    get_data(input$kpi, input$m, input$year)}
    ) %>% 
    # cache the data to avoid repetitive API
    bindCache(input$kpi, input$m, input$year)
  
  

  output$plot <- renderPlot({
    validate(
      need(is.data.frame(d()), "The inputs have no data")
    )
    
    ggplot(d(), aes(x = gender, y = value, fill=gender)) + 
      geom_bar(stat = "identity") +
      
      #print name of KPI --> Select one KPI group 
      labs(title = paste("Data for", kpi_members$member_title[kpi_members$member_id==input$kpi]),
           x = "Gender", y = "Values") +
      theme_bw()
  }) %>% 
    bindCache(d())
}











