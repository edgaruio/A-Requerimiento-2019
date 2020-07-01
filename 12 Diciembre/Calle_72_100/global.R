## ================== 0. Paquetes ------------------------------------------------
rm(list = ls())
# options(scipen = 999) 1018426823

library(shiny);library(dplyr)
library(shinydashboard);library(shinythemes)
library(DT);library(leaflet);library(shinyjs)
library(data.table);library(leaflet.extras)
library(rgdal);library(Hmisc);
library(tidyr); library(plotly)
library(shinycustomloader); library(scales)

# ================== 1. Datos ------------------------------------------------
# options(scipen = 999)
# set.seed(3.141593)

# Names
name_piramide1 <- toupper(c("Total","1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro"))
name_piramide2 <- toupper(c("Total","1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar","4.1 Estándar"))

empresas <- readRDS("Data/geo_empresas.rds") %>% 
  mutate(Piramide1 = toupper(Piramide1),
         Piramide2 = toupper(Piramide2),
         NumEmpleados = as.numeric(NumEmpleados))
str(empresas)

personas <- readRDS("Data/geo_personas.rds") %>% 
  mutate(smmlv = Salario/828116,
         smmlv2 = ifelse(smmlv >= 100, 100, smmlv),
         Piramide1 = toupper(Piramide1),
         Piramide2 = toupper(Piramide2),
         Segmento_poblacional = toupper(Segmento_poblacional),
         Categoria = as.character(Categoria))
str(personas)

# Names
name_categoria <- c("TOTAL","A","B","C")
name_segmento <- c("TOTAL","ALTO","JOVEN","MEDIO","BÁSICO")
name_poligono <- c("TOTAL", unique(empresas$cuadrante_emp))
name_poligono2 <- c("TOTAL", unique(empresas$cuadrante_emp))

# Leer poligonos
poligonos <- readRDS("Data/Capa_poligonos.rds")
class(poligonos)


