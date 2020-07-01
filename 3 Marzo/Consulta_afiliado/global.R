# Cargamos librerias
# options(scipen = 999)
rm(list = ls())
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes); library(tm)

# library(devtools)
# install_github("shinyGlobe", "trestletech")

persona <- fread("Data/Originales/Info_Ind.txt") %>% 
  data.frame() %>% 
  mutate(tipo_id = gsub("\\d","",id_persona))
str(persona)
# table(duplicated(persona$Id_Persona))

# test <- persona %>% 
#   sample_n(100,FALSE) %>% 
#   select(1)
# write.csv2(test, file = "Data/test_per.csv",row.names = F)
