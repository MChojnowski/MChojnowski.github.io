server <<- function(input, output, session) {
  

    data <- reactive({
      dane_COVID %>% 
      filter(db==input$AreaList) %>%
      arrange(Data) %>%
      mutate(Prognoza_Cum = cumsum(Prognoza)) 
    })

    data_SIR <- reactive({

      dane_SIR %>%
        filter(Area==input$AreaList) %>%
        mutate(Daily_Cases =  Cases - lag(Cases)) %>%
        mutate(Daily_Cases_plot =  Daily_Cases) %>%
        mutate(Daily_Cases = ifelse(is.na(Daily_Cases),0,Daily_Cases)) %>%
        mutate(Daily_Deaths =  Deaths - lag(Deaths)) %>%
        mutate(Daily_Deaths_plot = Daily_Deaths) %>%
        mutate(Daily_Deaths = ifelse(is.na(Daily_Deaths),0,Daily_Deaths)) %>%
        mutate(Cases_Wave1_plot = Cases_Wave1 * sum(Daily_Cases)) %>%
        mutate(Cases_Wave2_plot = Cases_Wave2 * sum(Daily_Cases)) %>%
        mutate(Cases_Wave3_plot = Cases_Wave3 * sum(Daily_Cases)) %>%
        mutate(Cases_Wave4_plot = Cases_Wave4 * sum(Daily_Cases)) %>%
        mutate(Cases_Wave_Total = Cases_Wave1+Cases_Wave2+Cases_Wave3+Cases_Wave4) %>%
        mutate(Cases_Wave_Total_plot = Cases_Wave_Total * sum(Daily_Cases)) %>%
        mutate(Deaths_Wave1_plot = Deaths_Wave1 * sum(Daily_Deaths)) %>%
        mutate(Deaths_Wave2_plot = Deaths_Wave2 * sum(Daily_Deaths)) %>%
        mutate(Deaths_Wave3_plot = Deaths_Wave3 * sum(Daily_Deaths)) %>%
        mutate(Deaths_Wave4_plot = Deaths_Wave4 * sum(Daily_Deaths)) %>%
        mutate(Deaths_Wave_Total = Deaths_Wave1+Deaths_Wave2+Deaths_Wave3+Deaths_Wave4) %>%
        mutate(Deaths_Wave_Total_plot = Deaths_Wave_Total * sum(Daily_Deaths))
    })
  
  data_decomposition <- reactive({
      
      data_SIR() %>%
      select(-ends_with("_plot")) %>%
      mutate(Deaths_Wave_Total = Deaths_Wave1 + Deaths_Wave2 + Deaths_Wave3 + Deaths_Wave4) %>%
      mutate(Fcst_Daily_Cases = Cases_Wave_Total * sum(Daily_Cases)) %>%
      mutate(Fcst_Daily_Cases = ifelse(is.na(Cases),Fcst_Daily_Cases,Daily_Cases)) %>%
      mutate(Fcst_Daily_Deaths = Deaths_Wave_Total * sum(Daily_Deaths)) %>%
      mutate(Fcst_Daily_Deaths = ifelse(is.na(Deaths),Fcst_Daily_Deaths,Daily_Deaths)) %>%
      mutate(Wave_1_Cases = Cases_Wave1/Cases_Wave_Total * Fcst_Daily_Cases) %>%
      mutate(Wave_2_Cases = Cases_Wave2/Cases_Wave_Total * Fcst_Daily_Cases) %>%
      mutate(Wave_3_Cases = Cases_Wave3/Cases_Wave_Total * Fcst_Daily_Cases) %>%
      mutate(Wave_4_Cases = Cases_Wave4/Cases_Wave_Total * Fcst_Daily_Cases) %>%
      mutate(Deaths_Wave_Total = Deaths_Wave1 + Deaths_Wave2 + Deaths_Wave3 + Deaths_Wave4) %>%
      mutate(Wave_1_Deaths = Deaths_Wave1/Deaths_Wave_Total * Fcst_Daily_Deaths) %>%
      mutate(Wave_2_Deaths = Deaths_Wave2/Deaths_Wave_Total * Fcst_Daily_Deaths) %>%
      mutate(Wave_3_Deaths = Deaths_Wave3/Deaths_Wave_Total * Fcst_Daily_Deaths) %>%
      mutate(Wave_4_Deaths = Deaths_Wave4/Deaths_Wave_Total * Fcst_Daily_Deaths)

    })
  
  data_wave_summary <- reactive({
    data_decomposition() %>%
      dplyr::select(starts_with("Wave_")) %>%
      gather() %>%
      mutate(Type=substring(key,sapply(gregexpr("_",key),function(x){return(x[2])})+1,nchar(key))) %>%
      mutate(ID=substring(key,1,sapply(gregexpr("_",key),function(x){return(x[2])})-1)) %>%
      dplyr::select(-one_of("key")) %>%
      group_by(ID, Type) %>%
      summarize(total=sum(value,na.rm=TRUE)) %>%
      spread(key=Type, value=total) %>%
      mutate(Cases=as.integer(Cases)) %>%
      mutate(Deaths=as.integer(Deaths)) %>%
      mutate(Mortality=paste0(round(Deaths/Cases*100,1),"%"))
  }) 

  data_SIR_summary <- reactive({
    data_SIR() %>%
      mutate(Wave_1_Cases = Cases_Wave1/Cases_Wave_Total * Daily_Cases) %>%
      mutate(Wave_2_Cases = Cases_Wave2/Cases_Wave_Total * Daily_Cases) %>%
      mutate(Wave_3_Cases = Cases_Wave3/Cases_Wave_Total * Daily_Cases) %>%
      mutate(Wave_4_Cases = Cases_Wave4/Cases_Wave_Total * Daily_Cases) %>%
      mutate(Wave_1_Deaths = Deaths_Wave1/Deaths_Wave_Total * Daily_Deaths) %>%
      mutate(Wave_2_Deaths = Deaths_Wave2/Deaths_Wave_Total * Daily_Deaths) %>%
      mutate(Wave_3_Deaths = Deaths_Wave3/Deaths_Wave_Total * Daily_Deaths) %>%
      mutate(Wave_4_Deaths = Deaths_Wave4/Deaths_Wave_Total * Daily_Deaths) %>%
      select(starts_with("Wave_")) %>%
      gather() %>%
      mutate(Type=substring(key,sapply(gregexpr("_",key),function(x){return(x[2])})+1,nchar(key))) %>%
      mutate(ID=substring(key,1,sapply(gregexpr("_",key),function(x){return(x[2])})-1)) %>%
      dplyr::select(-one_of("key")) %>%
      group_by(ID, Type) %>%
      summarize(total=sum(value,na.rm=TRUE)) %>%
      spread(key=Type, value=total) %>%
      mutate(Cases=as.integer(Cases)) %>%
      mutate(Deaths=as.integer(Deaths)) %>%
      mutate(Mortality=paste0(round(Deaths/Cases*100,1),"%"))
  }) 
  
  data_SIR_plot <- reactive({
    data_SIR_plot <- SIR_data %>%
      filter(Area==input$AreaList) %>%
      mutate(Active=Cases-Deaths-Recovered) %>%
      mutate(Daily_Cases =  Cases - lag(Cases)) %>%
      mutate(Daily_Deaths =  Deaths - lag(Deaths)) %>%
      mutate(Daily_Recovered =  Recovered - lag(Recovered)) %>%
      mutate(Wave_All_Cases = Cases_Wave1 + Cases_Wave2 + Cases_Wave3 + Cases_Wave4) %>%
      mutate(Wave_All_Deaths = Deaths_Wave1 + Deaths_Wave2 + Deaths_Wave3 + Deaths_Wave4) %>%
      mutate(Wave_All_Recovered = Recovered_Wave1 + Recovered_Wave2 + Recovered_Wave3 + Recovered_Wave4) %>%
      mutate(SIR_Cases_Wave1 = as.SIR(Daily_Cases,Cases_Wave1,Wave_All_Cases)) %>%
      mutate(SIR_Deaths_Wave1 = as.SIR(Daily_Deaths,Deaths_Wave1,Wave_All_Deaths)) %>%
      mutate(SIR_Recovered_Wave1 = as.SIR(Daily_Recovered,Recovered_Wave1,Wave_All_Recovered)) %>%
      mutate(SIR_Active_Wave1 = SIR_Cases_Wave1 - SIR_Deaths_Wave1 - SIR_Recovered_Wave1) %>%
      mutate(SIR_Cases_Wave2 = as.SIR(Daily_Cases,Cases_Wave2,Wave_All_Cases)) %>%
      mutate(SIR_Deaths_Wave2 = as.SIR(Daily_Deaths,Deaths_Wave2,Wave_All_Deaths)) %>%
      mutate(SIR_Recovered_Wave2 = as.SIR(Daily_Recovered,Recovered_Wave2,Wave_All_Recovered)) %>%
      mutate(SIR_Active_Wave2 = SIR_Cases_Wave2 - SIR_Deaths_Wave2 - SIR_Recovered_Wave2) %>%
      mutate(SIR_Cases_Wave3 = as.SIR(Daily_Cases,Cases_Wave3,Wave_All_Cases)) %>%
      mutate(SIR_Deaths_Wave3 = as.SIR(Daily_Deaths,Deaths_Wave3,Wave_All_Deaths)) %>%
      mutate(SIR_Recovered_Wave3 = as.SIR(Daily_Recovered,Recovered_Wave3,Wave_All_Recovered)) %>%
      mutate(SIR_Active_Wave3 = SIR_Cases_Wave3 - SIR_Deaths_Wave3 - SIR_Recovered_Wave3) %>%
      mutate(SIR_Cases_Wave4 = as.SIR(Daily_Cases,Cases_Wave4,Wave_All_Cases)) %>%
      mutate(SIR_Deaths_Wave4 = as.SIR(Daily_Deaths,Deaths_Wave4,Wave_All_Deaths)) %>%
      mutate(SIR_Recovered_Wave4 = as.SIR(Daily_Recovered,Recovered_Wave4,Wave_All_Recovered)) %>%
      mutate(SIR_Active_Wave4 = SIR_Cases_Wave4 - SIR_Deaths_Wave4 - SIR_Recovered_Wave4)
  })
  
  dane_DeathPDF_plot <- reactive({
    dane_DeathPDF_plot <- dane_DeathPDF %>% 
      filter(Area==input$AreaList) %>%
      gather(key="Wave",value="Prob",starts_with("Wave_"))
  })
  
  data_SIR_params <- reactive({
    data_SIR_params <- as.tibble(SIRparams) %>%
      mutate_at(.vars=c("beta","gamma","mu","Wave"),as.numeric)%>% 
      filter(Area==input$AreaList) %>%
      mutate(R0 = beta/(gamma+mu))
  })
  
  
  output$DailyPlot <- renderPlot({
    plotDataDaily <- data()
    
    ggplot(plotDataDaily, aes(x = Data)) +   
      geom_line(aes(y = Daily), colour = "grey") + 
      geom_line(aes(y = Prognoza), colour = "orange") + 
      geom_line(aes(y = MA7), colour = "navy") +
      theme(legend.position="bottom") +
      ylab("No. of cases daily")
    
  })
  
  output$CumPlot <- renderPlot({
    plotDataCum <- data()

    ggplot(plotDataCum, aes(x = Data)) +
      geom_line(aes(y = Prognoza_Cum), colour = "orange") +
      geom_line(aes(y = Cases), colour = "black", size=1.2) +
      theme(legend.position="bottom") +
      ylab("No. of cases")

  })
  
  output$HostPlot <- renderPlot({
    ggplot(data(), aes(x = Data)) +
      geom_line(aes(y = Hospitalized), colour = "black") +
      theme(legend.position="bottom") +
      ylab("No. of hospitalized")
    
  })
  
  output$WavesSummary <- renderTable({
    data_wave_summary()
  })
  
  output$WavesSummaryTotal <- renderTable({
    data_wave_summary() %>%
      ungroup() %>%
      summarize(Cases=sum(Cases),Deaths=sum(Deaths)) %>%
      mutate(Mortality=paste0(round(Deaths/Cases*100,1),"%")) %>%
      mutate(ID="Total") %>%
      select(ID,Cases,Deaths,Mortality)
  })
  
  output$SIRSummary <- renderTable({
    data_SIR_summary()
  })

  
  output$SIRSummaryTotal <- renderTable({
    data_SIR_summary() %>%
      ungroup() %>%
      summarize(Cases=sum(Cases),Deaths=sum(Deaths)) %>%
      mutate(Mortality=paste0(round(Deaths/Cases*100,1),"%")) %>%
      mutate(ID="Total") %>%
      select(ID,Cases,Deaths,Mortality)
  })
  
  output$SIRParams_W1 <- renderTable({
    data_SIR_params() %>%
      filter(Wave==1) %>%
      select(beta,gamma,mu,R0)
  })
  
  output$SIRParams_W2 <- renderTable({
    data_SIR_params() %>%
      filter(Wave==2) %>%
      select(beta,gamma,mu,R0)
  })
  
  output$SIRParams_W3 <- renderTable({
    data_SIR_params() %>%
      filter(Wave==3) %>%
      select(beta,gamma,mu,R0)
  })
  
  output$SIRParams_W4 <- renderTable({
    data_SIR_params() %>%
      filter(Wave==3) %>%
      select(beta,gamma,mu,R0)
  })
  
  output$WavePlotCases <- renderPlot({
    plotDataWave <- data_SIR()

    ggplot(plotDataWave, aes(x=Data)) +
      geom_line(aes(y=Daily_Cases_plot), colour="grey") +
      geom_line(aes(y=Cases_Wave1_plot), colour="black") +
      geom_line(aes(y=Cases_Wave2_plot), colour="black") +
      geom_line(aes(y=Cases_Wave3_plot), colour="black") +
      geom_line(aes(y=Cases_Wave4_plot), colour="black") +
      geom_line(aes(y=Cases_Wave_Total_plot), colour="orange")

  })

  output$WavePlotDeaths <- renderPlot({
    plotDataWave <- data_SIR()
    
    ggplot(plotDataWave, aes(x=Data)) +
      geom_line(aes(y=Daily_Deaths_plot), colour="grey") +
      geom_line(aes(y=Deaths_Wave1_plot), colour="black") +
      geom_line(aes(y=Deaths_Wave2_plot), colour="black") +
      geom_line(aes(y=Deaths_Wave3_plot), colour="black") +
      geom_line(aes(y=Deaths_Wave4_plot), colour="black") +
      geom_line(aes(y=Deaths_Wave_Total_plot), colour="orange")
    
  })
 
  output$DeathsPDFplot <- renderPlot({

    ggplot(dane_DeathPDF_plot(), aes(x=Days,y=Prob, fill=Wave)) +
      geom_bar(stat="identity", position = "identity", alpha=.3) 
    
  }) 
  
  output$SIRplotW1 <- renderPlot({
    plotData <- data_SIR_plot() %>%
      select(Data,ends_with("_Wave1"))%>%
      rename_all(~gsub("_Wave1","",make.names(.)))%>%
      mutate(Model_Cases = Model_Cases * max(SIR_Cases)) %>%
      mutate(Model_Active = Model_Active* max(SIR_Cases)) %>%
      mutate(Model_Recovered = Model_Recovered * max(SIR_Cases)) %>%
      mutate(Model_Deaths = Model_Deaths * max(SIR_Cases)) 
    
    ggplot(plotData,aes(x=Data)) +
      geom_line(aes(y=SIR_Cases), col="blue") + 
      geom_line(aes(y=Model_Cases), col="blue", linetype="dotdash") + 
      geom_line(aes(y=SIR_Deaths), col="red") + 
      geom_line(aes(y=Model_Deaths), col="red", linetype="dotdash") + 
      geom_line(aes(y=SIR_Recovered), col="chartreuse3") + 
      geom_line(aes(y=Model_Recovered), col="chartreuse3", linetype="dotdash") + 
      geom_line(aes(y=SIR_Active), col="orange") +
      geom_line(aes(y=Model_Active), col="orange", linetype="dotdash") 
    
  })
  
  output$SIRplotW2 <- renderPlot({
    plotData <- data_SIR_plot() %>%
      select(Data,ends_with("_Wave2"))%>%
      rename_all(~gsub("_Wave2","",make.names(.)))%>%
      mutate(Model_Cases = Model_Cases * max(SIR_Cases)) %>%
      mutate(Model_Active = Model_Active* max(SIR_Cases)) %>%
      mutate(Model_Recovered = Model_Recovered * max(SIR_Cases)) %>%
      mutate(Model_Deaths = Model_Deaths * max(SIR_Cases)) 
    
    ggplot(plotData,aes(x=Data)) +
      geom_line(aes(y=SIR_Cases), col="blue") + 
      geom_line(aes(y=Model_Cases), col="blue", linetype="dotdash") + 
      geom_line(aes(y=SIR_Deaths), col="red") + 
      geom_line(aes(y=Model_Deaths), col="red", linetype="dotdash") + 
      geom_line(aes(y=SIR_Recovered), col="chartreuse3") + 
      geom_line(aes(y=Model_Recovered), col="chartreuse3", linetype="dotdash") + 
      geom_line(aes(y=SIR_Active), col="orange") +
      geom_line(aes(y=Model_Active), col="orange", linetype="dotdash") 
    
  })
  
  output$SIRplotW3 <- renderPlot({
    plotData <- data_SIR_plot() %>%
      select(Data,ends_with("_Wave3"))%>%
      rename_all(~gsub("_Wave3","",make.names(.)))%>%
      mutate(Model_Cases = Model_Cases * max(SIR_Cases)) %>%
      mutate(Model_Active = Model_Active* max(SIR_Cases)) %>%
      mutate(Model_Recovered = Model_Recovered * max(SIR_Cases)) %>%
      mutate(Model_Deaths = Model_Deaths * max(SIR_Cases)) 
    
    ggplot(plotData,aes(x=Data)) +
      geom_line(aes(y=SIR_Cases), col="blue") + 
      geom_line(aes(y=Model_Cases), col="blue", linetype="dotdash") + 
      geom_line(aes(y=SIR_Deaths), col="red") + 
      geom_line(aes(y=Model_Deaths), col="red", linetype="dotdash") + 
      geom_line(aes(y=SIR_Recovered), col="chartreuse3") + 
      geom_line(aes(y=Model_Recovered), col="chartreuse3", linetype="dotdash") + 
      geom_line(aes(y=SIR_Active), col="orange") +
      geom_line(aes(y=Model_Active), col="orange", linetype="dotdash") 
    
  })
  
  output$SIRplotW4 <- renderPlot({
    plotData <- data_SIR_plot() %>%
      select(Data,ends_with("_Wave4"))%>%
      rename_all(~gsub("_Wave4","",make.names(.)))%>%
      mutate(Model_Cases = Model_Cases * max(SIR_Cases)) %>%
      mutate(Model_Active = Model_Active* max(SIR_Cases)) %>%
      mutate(Model_Recovered = Model_Recovered * max(SIR_Cases)) %>%
      mutate(Model_Deaths = Model_Deaths * max(SIR_Cases)) 
    
    ggplot(plotData,aes(x=Data)) +
      geom_line(aes(y=SIR_Cases), col="blue") + 
      geom_line(aes(y=Model_Cases), col="blue", linetype="dotdash") + 
      geom_line(aes(y=SIR_Deaths), col="red") + 
      geom_line(aes(y=Model_Deaths), col="red", linetype="dotdash") + 
      geom_line(aes(y=SIR_Recovered), col="chartreuse3") + 
      geom_line(aes(y=Model_Recovered), col="chartreuse3", linetype="dotdash") + 
      geom_line(aes(y=SIR_Active), col="orange") +
      geom_line(aes(y=Model_Active), col="orange", linetype="dotdash") 
    
  })
}
