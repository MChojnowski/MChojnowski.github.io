server <<- function(input, output) {
  
  data <- eventReactive (input$button, {
    data <- dane_COVID %>% 
      filter(db==input$AreaList)
    
    data
  })

  output$DailyPlot <- renderPlot({
    plotdata <- data()
    ggplot(plotdata, aes(x = Data)) +   
      geom_line(aes(y = Daily, colour = "Dzienne zachorowania")) + 
      geom_line(aes(y = Prognoza, colour = "Prognoza")) + 
      geom_line(aes(y = MA7, colour = "Wygladzone dzienne zachorowania"))
    
  })
}
