library(rsconnect)
library(tidyverse)
library(RSQLite)
library(RCurl)
library(shiny)

as.SIR <- function(daily,wave,total){
  x <- daily * wave / total
  x <- ifelse(is.na(x),0,x)
  x <- cumsum(x)
  
  return(x)
}

dane_COVID <- read_csv("https://mchojnowski.github.io/COVID/data.csv") %>%
              dplyr::select(-one_of("X1"))

dane_SIR <- read_csv("https://mchojnowski.github.io/COVID/SIR_PL.csv") %>%
              dplyr::select(-one_of("X1"))

dane_DeathPDF <- read_csv("https://mchojnowski.github.io/COVID/DeathPDF.csv") %>%
  dplyr::select(-one_of("X1"))

dane_SIRparams <- read_csv("https://mchojnowski.github.io/COVID/SIR_params.csv") %>%
  dplyr::select(-one_of("X1"))

print(head(dane_COVID))

source("ui.R", local=TRUE)
source("server.R", local=TRUE)
