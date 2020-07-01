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

cobranzas <- fread("BD/Originales/cobranzas_feb.csv") %>% 
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
  select(id_persona,TARJETA,Producto)
length(names(table(bd_sr$Producto)))
saveRDS(bd_sr, file = "BD/Depuradas/bd_sr.rds")

rm(list = ls())

# Cargamos base depurada
library(tidyr); library(dplyr)
bd_sr <- readRDS(file = "BD/Depuradas/bd_sr.rds") %>% 
  na.omit() %>% 
  mutate(valor = 1) %>%
  distinct() %>% 
  spread(key = Producto, value = valor, fill = 0) %>% 
  group_by(TARJETA) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  mutate_all(funs(ifelse(is.infinite(.),0,.))) %>% 
  select(-c(id_persona,`ALAMCEN POR DEPARTAMENTO`)) %>% 
  distinct()
str(bd_sr)
# bd_sr$Conteo = rowSums(bd_sr[,2:201])
# sum(bd_sr$Conteo == 0)
table(duplicated(bd_sr$TARJETA))

# Formato binarizado 
ratingmatrix = as(as.matrix(bd_sr[,-1]) , 'binaryRatingMatrix')


#### MODELOS ===================================================================================================================

# visualize the top matrix
min_prod <- quantile(rowCounts(ratingmatrix), 0.9999)
min_users <- quantile(colCounts(ratingmatrix), 0.971)

image(ratingmatrix[rowCounts(ratingmatrix) > min_prod, colCounts(ratingmatrix) > min_users], main = "Heatmap para Productos")

### Sistema de Recomendacion basado en contenido----
### Sistemas de Recomendacion Calibracion----
set.seed(31415)
e <- evaluationScheme(ratingmatrix[1:10000],method="cross-validation", k=3, given=-1)
n_recommendations <- c(1, 5, seq(10, 100, 10))

## UBCF User-based Collaborative Filtering

recommenderRegistry$get_entry("UBCF", dataType = "binaryRatingMatrix")

# Definicion de Modelos a Evaluar.
vec_nn <- c(5,20,50,100)
modelos_UBCF <- lapply(vec_nn, function(n){
  list(name = "UBCF", param = list(nn =n))
})
names(modelos_UBCF) <- paste0("UBCF_nn_", vec_nn)

# Evaluacion de Modelos
res_UBCF <- evaluate(e, method = modelos_UBCF, n= n_recommendations)

# Comparacion de Modelos
plot(res_UBCF, legend="topleft", annotate = F)
title("Curva ROC")
plot(res_UBCF, legend="topright", "prec/rec", annotate = F)
title("Precision / Recall")

# Metricas mejor Modelo
avg_matrices <- lapply(res_UBCF, avg)
avg_matrices$UBCF_nn_100[,5:8]

## IBCF Item-based Collaborative Filtering

recommenderRegistry$get_entry("IBCF", dataType = "binaryRatingMatrix")

# Definicion de Modelos a Evaluar.
vec_k <- c(1,5,10,20,50)
modelos_IBCF <- lapply(vec_k, function(k){
  list(name = "IBCF", param = list(k =k))
})
names(modelos_IBCF) <- paste0("IBCF_k_", vec_k)

# Evaluaci?n de Modelos
res_IBCF <- evaluate(e, method = modelos_IBCF, n= n_recommendations)

# Comparaci?n de Modelos
plot(res_IBCF, legend="topleft", annotate = F)
title("Curva ROC")
plot(res_IBCF, legend="topright", "prec/rec", annotate = F)
title("Precision / Recall")

# Metricas mejor Modelo
avg_matrices <- lapply(res_IBCF, avg)
avg_matrices$IBCF_k_5[,5:8]

## ALS Alternating Least Squares
recommenderRegistry$get_entry("ALS", dataType = "binaryRatingMatrix")

# Definicion de Modelos a Evaluar.
vec_nf <- c(1,5,10,20,50,100)
vec_l <- c(0.1,0.3,0.5,0.7,1)
modelos_ALS <- lapply(vec_l, function(n){
  list(name = "ALS", param = list(n_factors =20, lambda=vec_l))
})
names(modelos_ALS) <- paste0("ALS", vec_l)

# Evaluaci?n de Modelos
res_ALS <- evaluate(e, method = modelos_ALS, n= n_recommendations)

# Comparacion de Modelos
plot(res_ALS, legend="topleft", annotate = F)
title("Curva ROC")
plot(res_ALS, legend="topright", "prec/rec", annotate = F)
title("Precision / Recall")

# Metricas mejor Modelo
avg_matrices_ALS <- lapply(res_ALS, avg)
avg_matrices_ALS$ALS0.5[,5:8]

### Modelo Competencia ---- 

set.seed(31415)
e <- evaluationScheme(ratingmatrix[1:10000],  method="cross-validation", k=10, given=-1)
n_recommendations <- c(1, 5, seq(10, 100, 10))

algorithms <- list(
  RANDOM = list(name = "RANDOM"),
  POPULAR = list(name = "POPULAR"),
  UBCF = list(name = "UBCF", param = list(nn =100)),
  IBCF = list(name = "IBCF", param = list(k=50)),
  ALS = list(name = "ALS", param = list(n_factors = 20, lambda=0.7))
)
evlist <- evaluate(e, algorithms, n=seq(1, 100, 5))
plot(evlist, legend="topleft", annotate = F)
title("Curva ROC")
plot(evlist, legend="topright", "prec/rec", annotate = TRUE, main = "Precision-recall")
title("Precision / Recall")

class(evlist[[5]])

vec_nf <- c(20)
vec_l <- c(0.7)
modelos_ALS <- lapply(vec_l, function(n){
  list(name = "ALS", param = list(n_factors =20, lambda=vec_l))
})
names(modelos_ALS) <- paste0("ALS", vec_l)

# Evaluacion de Modelos
ALS <- evaluate(e, method = modelos_ALS, n= n_recommendations)


### RECOMENDACIONES ======
set.seed(31415)
recommender <- Recommender(ratingmatrix, method="IBCF", parameter=list(nn =50, method = "Jaccard"))
# recommender <- Recommender(ratingmatrix, method="POPULAR")
# recommender <- Recommender(ratingmatrix, method="ALS", parameter=list(n_factors =20, lambda=0.7))
predictions <- predict(recommender, ratingmatrix, n=10)
pred_list<- getList(predictions, decode=T)
dim(pred_list)

saveRDS(recommender, "Resultados/recommender_IBCF.rds")
saveRDS(pred_list, "Resultados/pred_list_IBCF.rds")

df <- data.frame(matrix(unlist(pred_list), nrow=length(pred_list), byrow=T))
str(df)

Predicciones <- as.data.frame(cbind(bd_sr[,1], df))
names(Predicciones) <- c("Tarjeta",paste0("Opcion",1:10))
str(Predicciones)

# recommender <- readRDS("Resultados/recommender_als.rds")


### DATA FINAL ======================================================================================================

data_info <- fread("BD/Originales/data.csv", na.strings = c("","na","NA")) %>% 
  filter(!duplicated(Tarjeta)) %>% 
  select(Tarjeta,Nombre.completo,ciudad,Saldo,salario,Segmento_Poblacion,Estado.cupo,AUTORIZACION) %>% 
  mutate(Tarjeta = as.character(Tarjeta))
str(data_info)
table(duplicated(data_info$Tarjeta))

consulta <- fread("BD/Originales/Consulta1.txt", sep = ";", dec = ",") %>% 
  data.frame() %>% 
  select(id_persona,Tarjeta,afiliado_mes_unico_edad,Genero) %>% 
  mutate(Tarjeta = as.character(Tarjeta)) %>% 
  dplyr::rename("edad"="afiliado_mes_unico_edad") %>% 
  distinct()
str(consulta)

persona <- readRDS("BD/Originales/Persona.rds") %>% 
  data.frame() %>% 
  select(Id_Persona,RangoEdad,Num_Hijos) %>% 
  distinct()
str(persona)

bd_recomendaciones <- Predicciones %>% 
  left_join(data_info,by = c("Tarjeta"="Tarjeta")) %>% 
  left_join(consulta, by = c("Tarjeta"="Tarjeta")) %>% 
  mutate(edad = round(ifelse(is.na(edad),mean(edad, na.rm = T),edad),1)) %>%
  mutate_at(c("Opcion1","Opcion2","Opcion3","Opcion4","Opcion5","Opcion6","Opcion7","Opcion8","Opcion9","Opcion10"),
            funs(as.character(.))) %>% 
  mutate_at(c("Opcion1","Opcion2","Opcion3","Opcion4","Opcion5","Opcion6","Opcion7","Opcion8","Opcion9","Opcion10"),
            funs(ifelse(.=="ALAMCEN POR DEPARTAMENTO","ALMACEN POR DEPARTAMENTO",.)))
str(bd_recomendaciones)

bd_recomendaciones <- bd_recomendaciones %>% 
  left_join(persona, by = c("id_persona"="Id_Persona")) %>% 
  mutate(Genero = ifelse(is.na(Genero),"Sin informacion",
                         ifelse(Genero == "M", "Masculino", "Femenino")),
         Num_Hijos = ifelse(is.na(as.character(Num_Hijos)),"Sin informacion",Num_Hijos))
str(bd_recomendaciones)
sum(is.na(bd_recomendaciones$Genero))
table(bd_recomendaciones$Genero)

saveRDS(bd_recomendaciones, file = "Protocolo/APP_SIS_RECOMEN/Data/data_recomendacion_pol.rds")
write.csv2(bd_recomendaciones, file = "Protocolo/APP_SIS_RECOMEN/Data/data_recomendacion_pol.csv", row.names = F)

# Grafico exploratorio
library(GGally)
library(gapminder)
library(plotly)
ggp1 <- ggpairs(bd_recomendaciones[,c(16,17,19)], title="Resumen", mapping = aes(color = Segmento_Poblacion),
                upper = list(continuous = "cor", combo = "box_no_facet", 
                             discrete = "facetbar"))
ggfin <- cowplot::plot_grid(ggp1[1,1],ggp1[1,3],ggp1[2,2],ggp1[2,3])
  
test <- info_ventas %>% 
  select(TARJETA, VALOR, Denominacion) %>% 
  group_by(TARJETA, Denominacion) %>% 
  summarise(VALOR = sum(VALOR),
            Conteo = n_distinct(TARJETA)) %>% 
  left_join(bd_recomendaciones %>% 
               select(Tarjeta,Segmento_Poblacion),
            by = c("TARJETA"="Tarjeta"))
  
ggp2 <- ggpairs(test[,c(2,3,5)], title="Resumen", mapping = aes(color = Denominacion),
                upper = list(continuous = "cor", combo = "box_no_facet", 
                             discrete = "facetbar"))

ggfin2 <- cowplot::plot_grid(ggp2[1,3])

# ggplotly(ggp2)
