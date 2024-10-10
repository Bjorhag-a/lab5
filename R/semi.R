library(ggplot2)
city <- c("Linköping", "Lund", "Mora")
year <- c("2017", "2018", "2019")
kpi <- c("how much", "etc")
gender <- c("M", "F", "T")
val <- c(10, 6, 8)

data <- data.frame(gender, val)

ggplot(data, aes(x = gender, y = val )) + 
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_bw()


library(shiny)
ui <- fluidPage(
  titlePanel("Kolada"),
  
  sidebarPanel(
    selectInput("m_id", "Select city", city),
    selectInput("year", "Select year", year),
    selectInput("kpi", "Select KPI", kpi)
    
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






x <- qbeta(0.05, 9,1)
x
