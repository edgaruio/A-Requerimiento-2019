# Datos

rm(list = ls())
library(RODBC); library(data.table); library(dplyr); library(tidyr)

conn_consulta_nit <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/MARZO/Consulta_NIT/Data/Originales/Consulta_Emp_Nit.accdb")
subset(sqlTables(conn_consulta_nit), tableType = "SYSTEM TABLE")
consulta_nit <- sqlFetch(conn_consulta_nit, "Info_Nit")
str(consulta_nit)
odbcClose(conn_consulta_nit)

bd_consulta_nit <- consulta_nit %>% 
  mutate_at(vars(id_empresa,razon_social,piramide_1,piramide_2,id_empresa_principal,nombre_empresa_principal), funs(as.character)) %>% 
  select(id_empresa,razon_social,piramide_1,piramide_2) %>% 
  filter(piramide_2 %in% c("1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver","3.1 VIP","3.2 VIP Estándar",
                           "4.1 Estándar","4.2 Trans. Mas de 100 Trab.","4.3 Trans.Juridica Ent. 11 a 99 Trab.",
                           "4.4 Trans.Natural Ent. 11 a 99 Trab.","4.5 Transaccional"))
str(bd_consulta_nit)

saveRDS(bd_consulta_nit, file = "Data/Originales/Consulta_BD_NIT.rds")
