ui <<- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("AreaList", "Select area", c("Poland","Mazowieckie","Warsaw"), multiple=FALSE, selectize=TRUE)
    ),
    
    
    # Main Panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Cases",
                    conditionalPanel(
                      condition = "nrow(data)>0",
                      h4("Summary"),
                      tableOutput("SIRSummary"),
                      tableOutput("SIRSummaryTotal"),
                      h4("Daily cases"),
                      plotOutput("DailyPlot"),
                      h4("Total cases"),
                      plotOutput("CumPlot"),
                      h4("Hospitalization"),
                      plotOutput("HostPlot"),
                    )
                  ),
                  tabPanel("Waves",
                           conditionalPanel(
                             condition = "nrow(data_SIR)>0",
                             h4("Wave Summary"),
                             tableOutput("WavesSummary"),
                             tableOutput("WavesSummaryTotal"),
                             h4("Wave decomposition - Cases"),
                             plotOutput("WavePlotCases"),
                             h4("Wave decomposition - Deaths"),
                             plotOutput("WavePlotDeaths"),
                             h4("Number of days between diagnosis and death (estimated)"),
                             h6("Poland only"),
                             plotOutput("DeathsPDFplot")
                           )
                  ),
                  tabPanel("SIRD",
                           conditionalPanel(
                             condition = "nrow(data)>0",
                             h6("Note: reuslts are derived from rough estimation, hence active cases at the end might not diverge to 0 (although should be close)"),
                             h4("Wave 1"),
                             tableOutput("SIRParams_W1"),
                             plotOutput("SIRplotW1"),
                             h4("Wave 2"),
                             tableOutput("SIRParams_W2"),
                             plotOutput("SIRplotW2"),
                             h4("Wave 3 - no forecast available yet"),
                             tableOutput("SIRParams_W3"),
                             plotOutput("SIRplotW3"),
                             h4("Wave 4 - no forecast available yet"),
                             tableOutput("SIRParams_W4"),
                             plotOutput("SIRplotW4")

                           )
                  )
                  )
      )
    )
)
