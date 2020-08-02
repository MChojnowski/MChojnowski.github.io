ui <<- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("AreaList", "Select area", c("Poland","Mazowieckie","Warsaw"), multiple=FALSE, selectize=TRUE),
      actionButton("button", "Show")
    ),
    
    # Main Panel
    mainPanel(
      conditionalPanel(
        condition = "nrow(data)>0",
        plotOutput("DailyPlot")
      )
      ),
    )
)
