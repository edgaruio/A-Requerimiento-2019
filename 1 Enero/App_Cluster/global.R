# Cargamos librerias

# options(scipen = 999)
rm(list = ls())
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(readxl)
# library(devtools)
# install_github("shinyGlobe", "trestletech")

empresas_prin <- fread("Data/EmpresaPrinicipal.csv", encoding = 'UTF-8')
str(empresas_prin)

info_empresas <- fread("Data/Info_emp_obj_21012019.csv")
str(info_empresas)

empresas_convenios <- fread("Data/bd_fin1.csv")
str(empresas_convenios)

empresas_portafolio <- fread("Data/bd_fin2.csv")
str(empresas_portafolio)

# name_empresa_prin <- unique(names(table(info_empresas[info_empresas$nombre != "#NAME?",]$nombre)))
# saveRDS(name_empresa_prin, file = "Data/name_empresa_prin.rds")
name_empresa_prin <- readRDS("Data/name_empresa_prin.rds")



# names(table(empresas_prin$piramide2))
