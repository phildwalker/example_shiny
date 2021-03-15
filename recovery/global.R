# import libraries and dataset
# Thu Oct 08 10:14:47 2020 ------------------------------

# ---- Library ----
library(treemapify)
library(tidyverse)
library(DT)
library(eastyle)
library(data.table)
library(dtplyr)
library(patchwork)
library(gtsummary)
library(gt)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
library(shinyWidgets)

# ---- Data ----
load(file = here::here("data-raw", "Comb_Forecast.rda"))
load(file = here::here("data-raw", "AggregHistDemo.rda"))
load(file = here::here("data-raw", "tmCat.rda"))

Comb_Forecast <-
  Comb_Forecast %>% 
  filter(!is.na(ComparisonFC)) %>% 
  full_join(., tmCat)


# ---- Settings ----
holdoutDT <- c("2020-03-01")
compDate <- c("2020-08-01")
# curWk <- as.Date("2021-01-17")
curWk <- lubridate::floor_date(Sys.Date(), "week")

MaxFlr <- max(Comb_Forecast$DT, na.rm=T)+1
MaxDt <- lubridate::ceiling_date(MaxFlr, "week")

