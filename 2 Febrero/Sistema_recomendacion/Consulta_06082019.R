# Librerias
rm(list = ls())
options(scipen = 999)
library(data.table); library(dplyr); library(tm); library(readxl); library(recommenderlab); library(tidyverse)
dir("BD/Originales/tms/")

# Leemos 2017
temp2017 <- list.files("BD/Originales/2017/",pattern = "+.csv", recursive = T)

AF2017<-fread(paste0("BD/Originales/2017/",temp2017[1]), sep=';', dec = ",", header = T, colClasses = c(rep("as.character",17)), encoding = 'UTF-8')
names(AF2017)<-c("CODIGO_ESTABLECIMIENTO","TARJETA","NOMBRE_ESTABLECIMIENTO","TRANSACCION","ORIGEN",
                 "FECHA","FECHA_PROCESO","VALOR","DESCRIPCION_TRANSACCION","UNIDAD_NEGOCIO",
                 "PLAZO","COD_AUTOR","VALOR_PROPINA","VALOR_COMPRA","VALOR_IVA","NIT","CANAL")
str(AF2017)
for (i in 2:length(temp2017)){
  tmp<-fread(paste0("BD/Originales/2017/",temp2017[i]),sep=';', dec = ",", header = T, colClasses = c(rep("as.character",17)), encoding = 'UTF-8')
  names(tmp)<-c("CODIGO_ESTABLECIMIENTO","TARJETA","NOMBRE_ESTABLECIMIENTO","TRANSACCION","ORIGEN",
                "FECHA","FECHA_PROCESO","VALOR","DESCRIPCION_TRANSACCION","UNIDAD_NEGOCIO",
                "PLAZO","COD_AUTOR","VALOR_PROPINA","VALOR_COMPRA","VALOR_IVA","NIT","CANAL")
  AF2017<-rbind(AF2017,tmp)
  # dim(tmp)
}
rm(tmp)

# Leemos 2018
temp2018 <- list.files("BD/Originales/2018/",pattern = "+.csv", recursive = T)

AF2018<-fread(paste0("BD/Originales/2018/",temp2018[1]), sep=';', dec = ",", header = T, colClasses = c(rep("as.character",17)), encoding = 'UTF-8')
names(AF2018)<-c("CODIGO_ESTABLECIMIENTO","TARJETA","NOMBRE_ESTABLECIMIENTO","TRANSACCION","ORIGEN",
                 "FECHA","FECHA_PROCESO","VALOR","DESCRIPCION_TRANSACCION","UNIDAD_NEGOCIO",
                 "PLAZO","COD_AUTOR","VALOR_PROPINA","VALOR_COMPRA","VALOR_IVA","NIT","CANAL")
str(AF2018)
for (i in 2:length(temp2018)){
  tmp<-fread(paste0("BD/Originales/2018/",temp2018[i]),sep=';', dec = ",", header = T, colClasses = c(rep("as.character",17)), encoding = 'UTF-8')
  names(tmp)<-c("CODIGO_ESTABLECIMIENTO","TARJETA","NOMBRE_ESTABLECIMIENTO","TRANSACCION","ORIGEN",
                "FECHA","FECHA_PROCESO","VALOR","DESCRIPCION_TRANSACCION","UNIDAD_NEGOCIO",
                "PLAZO","COD_AUTOR","VALOR_PROPINA","VALOR_COMPRA","VALOR_IVA","NIT","CANAL")
  AF2018<-rbind(AF2018,tmp)
  # dim(tmp)
}
rm(tmp)

# Leemos 2019
temp2019 <- list.files("BD/Originales/2019/",pattern = "+.csv", recursive = T)

AF2019<-fread(paste0("BD/Originales/2019/",temp2019[1]), sep=';', dec = ",",header = T, colClasses = c(rep("as.character",17)), encoding = 'UTF-8')
names(AF2019)<-c("CODIGO_ESTABLECIMIENTO","TARJETA","NOMBRE_ESTABLECIMIENTO","TRANSACCION","ORIGEN",
                 "FECHA","FECHA_PROCESO","VALOR","DESCRIPCION_TRANSACCION","UNIDAD_NEGOCIO",
                 "PLAZO","COD_AUTOR","VALOR_PROPINA","VALOR_COMPRA","VALOR_IVA","NIT","CANAL")
str(AF2019)
for (i in 2:length(temp2019)){
  tmp<-fread(paste0("BD/Originales/2019/",temp2019[i]),sep=';', dec = ",", header = T, colClasses = c(rep("as.character",17)), encoding = 'UTF-8')
  names(tmp)<-c("CODIGO_ESTABLECIMIENTO","TARJETA","NOMBRE_ESTABLECIMIENTO","TRANSACCION","ORIGEN",
                "FECHA","FECHA_PROCESO","VALOR","DESCRIPCION_TRANSACCION","UNIDAD_NEGOCIO",
                "PLAZO","COD_AUTOR","VALOR_PROPINA","VALOR_COMPRA","VALOR_IVA","NIT","CANAL")
  AF2019<-rbind(AF2019,tmp)
  # dim(tmp)
}
rm(tmp)

consumo_ind <- rbind(AF2017,AF2018,AF2019) %>% 
  data.frame()
str(consumo_ind)

# # Cargamos consumos
# 
# consumo_ind <- fread("BD/Originales/ventas.txt", sep = ";", dec = ",") %>%
#   data.frame() %>%
#   mutate(TARJETA = as.character(TARJETA),
#          NIT.ESTABLECIMIENTO = as.character(NIT.ESTABLECIMIENTO))
# str(consumo_ind)
# min(consumo_ind$FECHA)

tb_convenios <- fread("BD/Originales/TB_conv_ues.txt")
str(tb_convenios)

convenios_tms <- read_excel("BD/Originales/Convenios_TMS_febrero2019.xlsx") %>% 
  data.frame() %>% 
  select(NIT,RAZON.ZOCIAL,NOMBRE.DEL.COMERCIO,CATEGORIA,SUBCATEGORIA)
str(convenios_tms)

# Seleccionamos variables
info_ventas <- consumo_ind %>% 
  data.frame() %>% 
  select(NIT,TARJETA,NOMBRE_ESTABLECIMIENTO,UNIDAD_NEGOCIO,VALOR) %>% 
  left_join(tb_convenios, by = c("UNIDAD_NEGOCIO"="Div")) %>% 
  mutate(Denominacion = Denominación) %>% 
  group_by(TARJETA,NIT,NOMBRE_ESTABLECIMIENTO,UNIDAD_NEGOCIO,Denominacion) %>% 
  summarise(Conteo = n()) %>% 
  distinct() %>% 
  data.frame()
str(info_ventas)

info_sr <- info_ventas %>% select(-Conteo) %>% 
  left_join(convenios_tms %>% select(NIT,CATEGORIA,SUBCATEGORIA),
            by=c("NIT"="NIT")) %>% 
  mutate(Producto1 =  ifelse(is.na(SUBCATEGORIA), CATEGORIA, SUBCATEGORIA),
         Producto1 = ifelse(is.na(Producto1), Denominacion, Producto1)) %>%
  select(-c(CATEGORIA,SUBCATEGORIA)) %>%
  distinct() %>%
  mutate(Producto2 = ifelse(Denominacion=="CREDITO SOCIAL", NOMBRE_ESTABLECIMIENTO, Denominacion)) %>% 
  data.frame() %>% 
  mutate(Producto = ifelse(Producto1 == "CREDITO SOCIAL",Producto2,Producto1))
str(info_sr)
length(names(table(info_sr$Producto)))
sum(convenios_tms$NIT %in% info_ventas$NIT)

test_info <- info_sr %>%
  group_by(Producto1,Producto2) %>%
  summarise(conteo = n()) %>% 
  arrange(desc(conteo))

cobranzas <- fread("BD/Originales/cobranzas_julio.csv", encoding = 'UTF-8') %>% 
  data.frame() %>% 
  select(Tipo.Identificación,Nro.Identificación,Tarjeta,Saldo,Estado.Tarjeta,Ciudad) %>% 
  mutate(Tipo.Identificación = ifelse(Tipo.Identificación == 2, "CC", 
                                      ifelse(Tipo.Identificación == 3, "CE", "TI")),
         id_persona = paste(Tipo.Identificación,Nro.Identificación,sep = ""),
         Tarjeta =  as.character(Tarjeta)) %>% 
  select(id_persona,Tarjeta) %>% 
  distinct()
str(cobranzas)
colnames(cobranzas) <- chartr("áéíóú","aeiou",names(cobranzas))
str(cobranzas)

bd_sr <- info_sr %>%
  select(TARJETA, Producto) %>% 
  left_join(cobranzas,
            by=c("TARJETA"="Tarjeta")) %>% 
  select(id_persona,TARJETA,Producto) %>% 
  na.omit()
length(names(table(bd_sr$Producto)))
# saveRDS(bd_sr, file = "BD/Depuradas/bd_sr.rds")


afiliados <- fread("BD/originales/Base_mes_06082019.txt") %>% 
  data.frame() %>% 
  select(id_persona) %>% 
  distinct()
str(afiliados)

test_info <- bd_sr %>% 
  mutate(Afiliado = ifelse(bd_sr$id_persona %in% afiliados$id_persona, "Si", "No")) %>% 
  filter(Afiliado == "Si") %>% 
  filter(Producto %in% c("ACADEMIA DE IDIOMAS","EDUCACION (LIBROS, IDIOMAS Y UNIVERSIDADES)"))
str(test_info)
table(duplicated(test_info$id_persona))


# RECOMENDACIONES
reco_idiomas <- fread("BD/originales/Recomendacion2019-08-06 14_31_27.csv")
reco_unv_libros_idiomas <- fread("BD/originales/Recomendacion2019-08-06 14_31_42.csv")

recomendaciones <- rbind(reco_idiomas, reco_unv_libros_idiomas) %>% 
  select(Identificacion, Nombre.completo, Tarjeta) %>% 
  distinct() %>% 
  mutate(Afiliado = ifelse(Identificacion %in% afiliados$id_persona, "Si", "No")) %>% 
  filter(Afiliado == "Si") %>% 
  mutate(cruce = ifelse(Identificacion %in% test_info$id_persona, "Si", "No")) %>% 
  filter(cruce == "No")
str(recomendaciones)

fwrite(test_info, file = "BD/originales/consumos_idiomas.csv")
fwrite(recomendaciones, file = "BD/originales/recomendaciones_idiomas.csv")
sum(test_info$id_persona %in% recomendaciones$Identificacion)
