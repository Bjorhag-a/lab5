#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
ui <- fluidPage(
  titlePanel("Kolada"),
  
  sidebarPanel(
    selectInput("m_id", "Select city", choices = ),
    selectInput("kpi", "Select KPI", m_id),
    selectInput("year", "Select year", year),
    radioButtons("gender", "Select genders", gender)
    
  ),
  mainPanel(
    plotOutput("barplot")
  )
)


server <- function(input, output){
  
  output$plot <- renderPlot({
    ggplot(data, aes(x = gender, y = val )) + 
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_bw()
  }, height = 400, width = 400)
  
}




ui <- fluidPage(
  titlePanel("Kolada"),
  
  sidebarPanel(
    selectInput("m_id", "Select city", city),
    selectInput("year", "Select year", year),
    checkboxGroupInput("gender", "Select genders", gender)
    
  ),
  mainPanel(
    plotOutput("barplot")
  )
)


server <- function(input, output){
  
  output$plot <- renderPlot({
    ggplot(data, aes(x = gender, y = val )) + 
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_bw()
  }, height = 400, width = 400)
  
}
    
shinyApp(ui = ui, server = server)
  



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
