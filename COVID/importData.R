library(RSQLite)
library(tidyverse)
library(readxl)
library(deSolve)
library(lattice)
library(lubridate)

select <- dplyr::select

setwd("C:/Users/42874/OneDrive - Bain/PhD/Workspace/WWW/COVID")
plik <- "./Epidemia.xlsx"

##### SIR functions ######
SIR_model <- function(t,y,p){
  
  S <- y[1]
  I <- y[2]
  R <- y[3]
  
  alpha <- p[1]
  beta <- p[2]
  
  dS <- -alpha*S*I
  dI <- alpha*S*I - beta*I
  dR <- beta*I
  
  return(list(c(dS,dI,dR)))
  
}


SIRD_model <- function(t,y,p){
  
  S <- y[1]
  I <- y[2]
  R <- y[3]
  D <- y[4]
  
  alpha <- p[1]
  beta <- p[2]
  gamma <- p[3]
  
  dS <- -alpha*S*I
  dI <- alpha*S*I - beta*I - gamma*I
  dR <- beta*I
  dD <- gamma*I
  
  return(list(c(dS,dI,dR,dD)))
  
}

weighted_sq_mean <- function(x,y,w){
  
  ferrors <- (x-y)^2
  wmean <- sum(ferrors*w)/sum(w)
  
  return(wmean)
}

as.SIR <- function(daily,wave,total){
  x <- daily * wave / total
  x <- ifelse(is.na(x),0,x)
  x <- cumsum(x)
  
  return(x)
}

##### POLSKA #####
Polska <- read_xlsx(plik, sheet="Polska") %>%
          select(c("Data","Cases","Dead","Recovered","MultNormPred","Hospitalized"))

Polska <- Polska %>%
          mutate(Removed=Dead+Recovered) %>%
          mutate(Daily = Cases - lag(Cases)) %>%
          mutate(denominator=7-(is.na(lead(Daily,3))+is.na(lead(Daily,2))+is.na(lead(Daily,1))+is.na(Daily)+is.na(lag(Daily,1))+is.na(lag(Daily,2))+is.na(lag(Daily,3)))) %>%
          mutate(MA7 = (
              ifelse(is.na(lead(Daily,3)),0,lead(Daily,3)) + 
              ifelse(is.na(lead(Daily,2)),0,lead(Daily,2)) + 
              ifelse(is.na(lead(Daily,1)),0,lead(Daily,1))  + 
              Daily + 
              ifelse(is.na(lag(Daily,1)),0,lag(Daily,1)) +
              ifelse(is.na(lag(Daily,2)),0,lag(Daily,2)) +
              ifelse(is.na(lag(Daily,3)),0,lag(Daily,3)) 
          )/denominator) %>%
          mutate(db="Poland") %>%
          rename(Prognoza = MultNormPred) %>%
          filter(!is.na(Prognoza)) %>%
          select(-one_of("denominator"))

Mazowsze <- read_xlsx(plik, sheet="Mazowsze") %>%
            select(c("Date","Cases","Fcst","Hospitalized")) %>%
            mutate(Daily = Cases - lag(Cases)) %>%
            mutate(denominator=7-(is.na(lead(Daily,3))+is.na(lead(Daily,2))+is.na(lead(Daily,1))+is.na(Daily)+is.na(lag(Daily,1))+is.na(lag(Daily,2))+is.na(lag(Daily,3)))) %>%
            mutate(MA7 = (
              ifelse(is.na(lead(Daily,3)),0,lead(Daily,3)) + 
                ifelse(is.na(lead(Daily,2)),0,lead(Daily,2)) + 
                ifelse(is.na(lead(Daily,1)),0,lead(Daily,1))  + 
                Daily + 
                ifelse(is.na(lag(Daily,1)),0,lag(Daily,1)) +
                ifelse(is.na(lag(Daily,2)),0,lag(Daily,2)) +
                ifelse(is.na(lag(Daily,3)),0,lag(Daily,3)) 
            )/denominator) %>%
            mutate(db="Mazowieckie") %>%
            rename(Prognoza = Fcst) %>%
            rename(Data = Date) %>%
            select(-one_of("denominator"))
  
Warszawa <- read_xlsx(plik, sheet="Warszawa") %>%
            select(c("Date","Cases","Deaths","Recoveries","Hospitalized","HomeIzolation","Fcst")) %>%
            mutate(Daily = Cases - lag(Cases)) %>%
            mutate(denominator=7-(is.na(lead(Daily,3))+is.na(lead(Daily,2))+is.na(lead(Daily,1))+is.na(Daily)+is.na(lag(Daily,1))+is.na(lag(Daily,2))+is.na(lag(Daily,3)))) %>%
            mutate(MA7 = (
              ifelse(is.na(lead(Daily,3)),0,lead(Daily,3)) + 
                ifelse(is.na(lead(Daily,2)),0,lead(Daily,2)) + 
                ifelse(is.na(lead(Daily,1)),0,lead(Daily,1))  + 
                Daily + 
                ifelse(is.na(lag(Daily,1)),0,lag(Daily,1)) +
                ifelse(is.na(lag(Daily,2)),0,lag(Daily,2)) +
                ifelse(is.na(lag(Daily,3)),0,lag(Daily,3)) 
            )/denominator) %>%
            mutate(Hospitalized = Hospitalized + HomeIzolation) %>%
            mutate(db="Warsaw") %>%
            rename(Prognoza = Fcst) %>%
            rename(Data = Date) %>%
            select(-one_of("denominator"))

Polska2DB <- Polska %>%
            select(c("db","Data","Cases","Daily","Prognoza","MA7","Hospitalized"))
MZ2DB <- Mazowsze %>%
            select(c("db","Data","Cases","Daily","Prognoza","MA7","Hospitalized"))
WWA2DB <- Warszawa %>%
            select(c("db","Data","Cases","Daily","Prognoza","MA7","Hospitalized"))

downloadable_data <- rbind(Polska2DB,MZ2DB,WWA2DB)


SIR_Polska <- read_xlsx(plik, sheet="SIRdata_PL") %>%
              mutate(Area="Poland") %>%
              select(-ends_with("_Daily"))

SIR_Mazowieckie <- read_xlsx(plik, sheet="SIRdata_MZ") %>%
  mutate(Area="Mazowieckie")

SIR_Warszawa <- read_xlsx(plik, sheet="SIRdata_WWA") %>%
  mutate(Area="Warsaw")

SIR_data <- rbind(SIR_Polska, SIR_Mazowieckie, SIR_Warszawa) %>%
              mutate(Data=as.Date(Data))

Countdown_PL <- read_xlsx(plik, sheet="Wave_Allignment", range ="K1:N36") %>%
  mutate(Area="Poland")

Countdown_MZ <- read_xlsx(plik, sheet="Wave_Allignment_MZ", range ="K1:N36") %>%
  mutate(Area="Mazowieckie")

Countdown_WWA <- read_xlsx(plik, sheet="Wave_Allignment_WWA", range ="K1:N36") %>%
  mutate(Area="Warsaw")


Countdown <- rbind(Countdown_PL,Countdown_MZ,Countdown_WWA)

###### SIR ######
SIRparams <- NULL
SIRest <- NULL

for (areas in c("Poland","Mazowieckie","Warsaw")){
  data_SIR_plot <- SIR_data %>%
    filter(Area==areas) %>%
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
  
    for (w_i in 1:3){
    fala <- paste0("_Wave",w_i)
    modelData <- data_SIR_plot %>%
                select(ends_with(fala)) %>%
                select(starts_with("SIR")) %>%
                rename_all(~gsub(fala,"",.)) %>%
                rename_all(~gsub("SIR_","",.)) %>%
                mutate(Active = Active / max(Cases)) %>%
                mutate(Deaths = Deaths / max(Cases)) %>%
                mutate(Recovered = Recovered / max(Cases)) %>%
                mutate(Cases = 1-(Cases / max(Cases))) %>%
                mutate(Removed = Deaths + Recovered)
    
    y0org <- as.matrix(select(modelData,Cases,Active,Removed,Recovered,Deaths)[min(which(modelData$Cases<1)),])
    y0 <- y0org[,c("Cases","Active","Removed")]
    names(y0) <- c("S","I","R")
    
    Tmin <- which(diff(modelData$Cases)==0)
    Tmin <- min(Tmin[Tmin>25])
    
    benchmark <- Inf
    ERRORS <- NULL
    for (alpha_loop in seq(0.01,1,0.01)){
      for (beta_loop in seq(0.01,alpha_loop,0.01)){
        
        params <- c(alpha=alpha_loop,beta=beta_loop)
        
        model <- ode(y=y0, times=seq(min(which(modelData$Cases<1)),Tmin+1,1), func=SIR_model, parms=params)
        
        if (w_i<3){
          try(model <- model[-(1:(which(model[,2]<0.5)[1]-which(modelData$Cases<0.5)[1])),],silent=TRUE)
          try(model <- model[-(1:(which(modelData$Cases<0.5)[1]-which(model[,2]<0.5)[1])),],silent=TRUE)
        }
        
        error <- 
          sqrt(weighted_sq_mean(
            model[,2]
            ,modelData$Cases[model[,1]]
            ,modelData$Cases[model[,1]]
          )) +
          sqrt(weighted_sq_mean(
            model[,4]
            ,modelData$Removed[model[,1]]
            ,modelData$Removed[model[,1]]
          ))
           
        ERRORS <- rbind(ERRORS,c(error,alpha_loop,beta_loop))
        if (error<benchmark){
          benchmark <- error
          alpha_star <- alpha_loop
          beta_star <- beta_loop
        }
      }}
    
    #####

    model <- ode(y=y0, times=seq(min(which(modelData$Cases<1)),Tmin+1,1), func=SIR_model, parms=c(alpha_star,beta_star))
    
    ERRORS <- as_tibble(ERRORS)
    colnames(ERRORS) <- c("error","alpha","beta")
    ggplot(ERRORS,aes(x=alpha,y=beta)) +
      geom_tile(aes(fill=error),interpolate=TRUE)
      
    
    #####
    
    y0 <- y0org[,c("Cases","Active","Recovered","Deaths")]
    names(y0) <- c("S","I","R","D")
    benchmark <- Inf
    
    for (gamma_loop in seq(0.0001,beta_star,0.0001)){
      params <- c(alpha=alpha_star,beta=beta_star-gamma_loop,gamma=gamma_loop,thres=1)
      
      model <- ode(y = y0, seq(min(which(modelData$Cases<1)),Tmin+1,1), SIRD_model, params)
      
      if (w_i<3){
        try(model <- model[-(1:(which(model[,2]<0.5)[1]-which(modelData$Cases<0.5)[1])),],silent=TRUE)
        try(model <- model[-(1:(which(modelData$Cases<0.5)[1]-which(model[,2]<0.5)[1])),],silent=TRUE)
      }
      
      error <- 
        sqrt(weighted_sq_mean(
          model[,4]
          ,modelData$Recovered[model[,1]]
          ,modelData$Recovered[model[,1]]
        )) + sqrt(weighted_sq_mean(
          model[,5]
          ,modelData$Deaths[model[,1]]
          ,modelData$Deaths[model[,1]]
        ))
    
      if (error<benchmark){
        benchmark <- error
        gamma_star <- gamma_loop
      }
    }
  
    model <- ode(y = y0, seq(min(which(modelData$Cases<1)),Tmin+1,1), SIRD_model, c(alpha_star,beta_star,gamma_star))
    
    if (w_i<3){
      try(model <- model[-(1:(which(model[,2]<0.5)[1]-which(modelData$Cases<0.5)[1])),],silent=TRUE)
      try(model <- model[-(1:(which(modelData$Cases<0.5)[1]-which(model[,2]<0.5)[1])),],silent=TRUE)
    }
    
    SIRparams <- rbind(SIRparams,
                       c(alpha_star,beta_star,gamma_star,areas,w_i))
    SIRest <- rbind(SIRest,
                    cbind(model,areas,w_i))
    
  } # End for w_i 
} # End for areas


###### Add model to SIR_data
SIR_data <- as_tibble(SIRest) %>%
            mutate(time=as.integer(time)) %>%
            mutate(Model_Cases=1-as.double(S)) %>%
            mutate(Model_Active=as.double(I)) %>%
            mutate(Model_Deaths=as.double(R)) %>%
            mutate(Model_Recovered=as.double(D)) %>%
            mutate(Data=as.Date("2020-03-04")+days(time-1)) %>%
            mutate(w_i=paste0("Wave",w_i)) %>%
            rename(Area=areas) %>%
            select("Data","Model_Cases","Model_Active","Model_Deaths","Model_Recovered","Area","w_i") %>%
            pivot_wider(values_from = c("Model_Cases","Model_Active","Model_Deaths","Model_Recovered"), names_from="w_i") %>%
            inner_join(SIR_data,.,by=c("Data","Area"))


colnames(SIRparams) <- c("beta","gamma","mu","Area","Wave")

write.csv(downloadable_data,"data.csv")
write.csv(SIR_data,"SIR_PL.csv")
write.csv(Countdown,"DeathPDF.csv")
write.csv(SIRparams,"SIR_params.csv")
