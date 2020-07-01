## ================== 0. Paquetes ------------------------------------------------
rm(list = ls())
# options(scipen = 999) 1018426823

library(shiny);library(ggplot2);library(dplyr);library(ggmap)
library(maptools);library(maps);library(shinydashboard);library(shinythemes)
library(DT);library(leaflet);library(shinyjs);library(reshape); library(rgdal)
library(data.table); library(readxl); library(leaflet.extras);
library(Hmisc); library(tidyr); library(janitor); library(readxl); library(plotly)

# .Deprecated("add_totals_row")
# .Deprecated("add_totals_col")

# ================== 1. Manipulacion de datos ------------------------------------------------


# ================== 2. Preparacion Shiny ------------------------------------------------
options(scipen = 999)

edadGF <- readRDS("Data/GrupoF_spread.rds")
str(edadGF)

data <- readRDS("Data/Calificada_04012019_V2.rds") %>% 
  left_join(edadGF, by = c("Identificacion"="id_persona"))
str(data)

data_rec <- readRDS("Data/data_recomendacion_pol.rds")
str(data_rec)

# edadGF <- readRDS("Data/GrupoF.rds") %>% 
#   data.frame() %>% 
#   select(id_persona,Edad) %>% 
#   na.omit() %>% 
#   mutate(Edad_hijo = cut2(Edad,g = 5),
#          Edad_hijo = ifelse(Edad_hijo == "[ 0,  6)", "Entre 0 y 5",
#                             ifelse(Edad_hijo == "[ 6, 10)", "Entre 6 y 9",
#                                    ifelse(Edad_hijo == "[10, 15)", "Entre 10 y 14",
#                                           ifelse(Edad_hijo == "[15, 19)", "Entre 15 y 18","Mas de 19"))))) %>% 
#   group_by(id_persona,Edad_hijo) %>%
#   summarise(Conteo = n()) %>%
#   spread(key = Edad_hijo, value = Conteo, fill = 0)
# str(edadGF)
# saveRDS(edadGF, file = "Data/GrupoF_spread.rds")



# test_producto <- data_rec %>%
#   select(Opcion1:Opcion10) %>%
#   gather(factor_key = "Producto", value = "Conteo", 1:10) %>%
#   group_by(Conteo) %>%
#   summarise(n_cuenta = n())
# 
# name_producto <- unique(names(table(test_producto$Conteo)))
# saveRDS(name_producto,"Data/name_producto.rds")

name_producto <- readRDS("Data/name_producto.rds")
name_producto <- c("Total",name_producto)

id_cc <- as.character(data_rec$id_persona)

convenios <- read_excel("Data/Convenios_TMS.xlsx") %>% 
  data.frame() %>% 
  select(RAZON.SOCIAL,CATEGORIA,CX,CY)

ventas <- fread("Data/ventas_agru.csv") %>% 
  data.frame() %>% 
  mutate(TARJETA =as.character(TARJETA))

# Nombres
name_piramide1 <- c("1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro")
name_piramide2 <- c("Total","1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estandar","4.1 Estándar")
name_segmento1 <- c("Total","Alto","Joven","Medio","Básico")
name_segmento2 <- c("Total","Alto","Joven","Medio","Básico")
name_categoria <- c("Total","A","B","C")
name_genero <- c("Total","Sin informacion", "Masculino", "Femenino")

# edad_hijos <- unique(names(table(edadGF$Edad_hijo)))
# saveRDS(edad_hijos, "Data/edad_hijos.rds")

# Capas
cundi <- readRDS("Data/Cundinamarca.rds")
localidad <- readOGR("Data/poligonos-localidades.shp")



# Animals <- c("giraffes", "orangutans", "monkeys")
# SF_Zoo <- c(20, 14, 23)
# LA_Zoo <- c(12, 18, 29)
# data <- data.frame(Animals, SF_Zoo, LA_Zoo)

# # grafico recomenadaciones
# 
# # Recomendacion 1
# library(plotly)
# grap1 <-  data_rec %>% 
#   # filter(edad %in% input) %>% 
#   select(Opcion1) %>% 
#   group_by(Opcion1) %>% 
#   summarise(Conteo = n()) %>% 
#   arrange(desc(Conteo))
# 
# p <- plot_ly(grap1, x = ~Conteo, y = ~reorder(Opcion1,Conteo), type = 'bar', orientation = 'h'
#              #,
#              # marker = list(color = 'rgb(158,202,225)',
#              #               line = list(color = 'rgb(8,48,107)',
#              #                           width = 1.5))
#              ) %>%
#   layout(title = "Primera Recomendacion",
#          xaxis = list(title = ""),
#          yaxis = list(title = ""))
# p
# 
# data_grap <- data_rec %>% 
#   select(Opcion1:Opcion10) %>% 
#   gather(factor_key = "Producto", value = "Recomendacion", 1:10) %>% 
#   dplyr::rename("Producto"="1:10") %>% 
#   group_by(Producto,Recomendacion) %>% 
#   summarise(Conteo = n()) %>% 
#   arrange(Producto,desc(Conteo)) %>% 
#   data.frame()
# str(data_grap)  

