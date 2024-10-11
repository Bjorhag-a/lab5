server <-
function(input, output){
  
  output$plot <- renderPlot({
    ggplot(data, aes(x = gender, y = val )) + 
      geom_bar(stat = "identity", fill = "lightblue") +
      theme_bw()
  }, height = 400, width = 400)
  
}
