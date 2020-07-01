# Datos

rm(list = ls())
library(RODBC); library(data.table); library(dplyr); library(tidyr)

conn_consulta_ind <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/MARZO/Consulta_Afiliado/Data/Originales/Consulta_BD_IND.accdb")
subset(sqlTables(conn_consulta_ind), tableType = "SYSTEM TABLE")
consulta_ind <- sqlFetch(conn_consulta_ind, "Info_Ind")
str(consulta_ind)
odbcClose(conn_consulta_ind)

bd_consulta_ind <- consulta_ind %>% 
  mutate_at(vars(id_persona,id_empresa,razon_social,id_empresa_principal,nombre_empresa_principal,Segmento_poblacional,categoria), funs(as.character)) %>% 
  select(id_persona,id_empresa,razon_social,id_empresa_principal,nombre_empresa_principal,Segmento_poblacional,categoria) %>% 
  mutate(tipo_id = gsub("\\d","",id_persona))
str(bd_consulta_ind)

saveRDS(bd_consulta_ind, file = "Data/Originales/Consulta_BD_IND.rds")
