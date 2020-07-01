# Cargamos librerias
# options(scipen = 999)
rm(list = ls())
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes)

# library(devtools)
# install_github("shinyGlobe", "trestletech")

# Cargamos bases de datos
bd_empresas <- fread("Data/Info_Nit.txt") %>% 
  data.frame() %>% 
  select(id_empresa,razon_social,piramide_1,piramide_2)
str(bd_empresas)

# name_tipo_nit <- c("Principal","Secundaria")
name_piramide1 <- c("Total","1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro")
name_piramide2 <- c("Total","1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
                    "4.1 Estándar","4.2 Trans. Mas de 100 Trab.","4.3 Trans.Juridica Ent. 11 a 99 Trab.",
                    "4.4 Trans.Natural Ent. 11 a 99 Trab.","4.5 Transaccional")

# # Consulta
# 
# consulta <- fread("Data/consulta nit.csv") %>% 
#   data.frame() %>% 
#   mutate(nit_digito = gsub("\\D","",Nit)) %>% 
#   mutate(primer_dig = substr(nit_digito, 1, 1)) %>% 
#   mutate(nit_2 = ifelse(primer_dig %in% c("8","9"), "NIT", "CC")) %>% 
#   mutate(id_empresa = paste0(nit_2,nit_digito))
# str(consulta)
# 
# consulta <- consulta %>% 
#   left_join(bd_empresas, by = "id_empresa") %>% 
#   select(-nit_2) %>% 
#   mutate(Estado = ifelse(is.na(razon_social),"No afiliada", "Afiliada"))
# str(consulta)
# sum(is.na(consulta$razon_social))
# 
# fwrite(consulta, file = "Data/Consulta_Nit.csv", row.names = F)
