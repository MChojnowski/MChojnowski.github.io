library(RSQLite)
library(tidyverse)
library(readxl)

plik <- "./Epidemia.xlsx"

##### POLSKA #####
Polska <- read_xlsx(plik, sheet="Polska") %>%
          select(c("Data","Cases","Dead","Recovered","MultNormPred"))

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
            select(c("Date","Cases","Fcst")) %>%
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
            mutate(Hospitalized_total = Hospitalized + HomeIzolation) %>%
            mutate(db="Warsaw") %>%
            rename(Prognoza = Fcst) %>%
            rename(Data = Date) %>%
            select(-one_of("denominator"))

Polska2DB <- Polska %>%
            select(c("db","Data","Cases","Daily","Prognoza","MA7"))
MZ2DB <- Mazowsze %>%
            select(c("db","Data","Cases","Daily","Prognoza","MA7"))
WWA2DB <- Warszawa %>%
            select(c("db","Data","Cases","Daily","Prognoza","MA7"))

downloadable_data <- rbind(Polska2DB,MZ2DB,WWA2DB)

write.csv(downloadable_data,"data.csv")
