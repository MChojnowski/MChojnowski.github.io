server <<- function(input, output) {
  
  data <- eventReactive (input$button, {
    if (input$AreaList=="Poland"){
      data <- Polska
    } else if (input$AreaList=="Mazowieckie"){
      data <- Mazowsze
    } else if (input$AreaList=="Warsaw"){
      data <- Warszawa  
    }
  })

  output$DailyPlot <- renderPlot({
    plotdata <- data()
    ggplot(plotdata, aes(x = Data)) +   
      geom_line(aes(y = Daily, colour = "Dzienne zachorowania")) + 
      geom_line(aes(y = Prognoza, colour = "Prognoza")) + 
      geom_line(aes(y = MA7, colour = "Wygladzone dzienne zachorowania"))
    
  })
}
