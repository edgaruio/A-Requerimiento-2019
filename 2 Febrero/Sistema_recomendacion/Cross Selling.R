rm(list=ls(all=T))
# source("~/CamiloYate/Funciones.r")
# pkgs <- c("recommenderlab", "tidyverse", "data.table")
# Loadpkg(pkgs)
library(recommenderlab); library(tidyverse); library(data.table)

### Cargue y depuracion de la base de datos ----

# Seguros 
seguros <- fread("BD/Originales/VigentesCancelados.csv") %>% 
  mutate(id_persona=paste0(`TIPO DE INDENTIFICACIÓN`, NÚMERO)) %>% 
  select(id_persona) %>% unique()
str(seguros)

#TMS
cobranzas <- fread("BD/Originales/Cobranzas.csv") %>% unique()

# Piscilago
pisci <-  readRDS("BD/Originales/Data_final.rds") %>%  select(IdPersona) %>% unique()
str(pisci)

data <- fread("BD/Originales/agrupacion.txt") %>% select(-Parentesco,-id_grupo_familiar) %>% 
  group_by(id_afiliado) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  mutate_all(funs(ifelse(is.infinite(.),0,.))) %>% 
  mutate(seguro=ifelse(id_afiliado %in% seguros$id_persona, 1,0),
         `cupo titular`=ifelse(id_afiliado %in% cobranzas$id_persona, 1,0),
         piscilago=ifelse(id_afiliado %in% pisci$IdPersona, 1,0))
str(data)
fwrite(data, "BD/Depuradas/datta.csv")
data <- fread("BD/Depuradas/datta.csv")


rm(cobranzas, seguros, pisci)
ratingmatrix = as(as.matrix(data[,-1]) , 'binaryRatingMatrix')

### Sistema de Recomendacion basado en contenido----

set.seed(31415)
recommender <- Recommender(ratingmatrix, method="RANDOM")
predictions <- predict(recommender, ratingmatrix, n=10)
pred_list<- getList(predictions, decode=T)

saveRDS(recommender, "Resultados/recommender.rds")

Predicciones <- as.data.frame(cbind(data[,1], do.call(rbind, pred_list)))
names(Predicciones) <- c( "id_persona", paste0("Opcion",1:10))
rm(data, predictions, ratingmatrix, pred_list)

### Preparacion Shiny ----

fecha=as.Date("2018-08-31")

cuota <- fread("BD/Originales/acumulado descuentos con cuota.txt") %>% 
  select(NoIdentAfiliado) %>% unique()
str(cuota)

persona <- readRDS("BD/Originales/Consolidada.rds") %>% 
  filter(Per_ind_segemento_emp=="X") %>% 
  select(id_persona, idempresa, Per_piramide1, Per_piramide2, segmento_poblacional, categoria, total_numero_grupo_familiar, 
         fecha_nacimiento, genero, razonsocial) %>% 
  mutate(fecha_nacimiento=as.Date(fecha_nacimiento, "%d/%m/%Y"),
         Edad=Calcular.Edad(fecha_nacimiento, fecha),
         Cuota=ifelse(as.numeric(gsub("\\D","",id_persona)) %in% cuota$id_persona, "Si", "No"),
         Colsubsidio=ifelse(Per_piramide2=="4.9 Colsubsidio", "Si", "No"),
         PAC=ifelse(total_numero_grupo_familiar>4, "Más de 4", as.character(total_numero_grupo_familiar)))
str(persona)

BD <- Predicciones %>% left_join(persona)
fwrite(BD, "BD/Depuradas/BD.csv")
saveRDS(BD, "Protocolo/APP_Cross_Selling/data/BD.rds")

BD <- readRDS("Protocolo/APP_Cross_Selling/data/BD.rds") %>% 
  mutate(ifelse(segmento_poblacional %in% c("Alto","Básico","Medio"), segmento_poblacional, "Joven"))

BD$segmento_poblacional =ifelse(BD$segmento_poblacional %in% c("Alto","Básico","Medio"), BD$segmento_poblacional, "Joven")
BD$segmento_poblacional =ifelse(BD$segmento_poblacional =="Básico", "Basico", BD$segmento_poblacional)
table(BD$segmento_poblacional)

BD$Opcion1=ifelse(BD$Opcion1=="minibar","alojamiento", BD$Opcion1)

Empresas <- unique(BD$razonsocial)
pir1 <- unique(BD$Per_piramide1)
pir2 <- unique(BD$Per_piramide2)
rm(cuota, persona, Predicciones, recommender)
