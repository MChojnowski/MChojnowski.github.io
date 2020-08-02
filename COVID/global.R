library(rsconnect)
library(tidyverse)
library(RSQLite)
library(RCurl)

dane_COVID <- read_csv("https://mchojnowski.github.io/COVID/data.csv") %>%
              select(-one_of("X"))

source("ui.R", local=TRUE)
source("server.R", local=TRUE)
