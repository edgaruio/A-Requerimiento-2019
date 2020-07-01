# Librearias
rm(list = ls())
library(dplyr); library(data.table); library(readxl); library(tidyr); library(purrr); library(stringr); library(tm)

# Datos
source("Funciones.R")

empresas1 <- fread("EmpresaPrinicipal.csv", encoding = 'UTF-8') %>% data.frame() %>% 
  filter(piramide2 %in% c("1.1 Platinum","1.2 Premium","2.1 Gold","2.2 Silver")) %>% 
  distinct()
str(empresas1)

empresas2 <- fread("Info_act_21012018.csv") %>% 
  select(id_empresa_principal,nombre_empresa_principal) %>% 
  distinct()
str(empresas2)

empresas <- left_join(empresas1, empresas2, by = c("idnitprincipal"="id_empresa_principal")) %>% 
  select(idnitprincipal,nombre_empresa_principal,piramide1:cluster)
str(empresas)

# empresas_fin <- empresas %>% 
#   mutate(cluster = ifelse(P_basico >= .65, "1. Basico" ,
#                           ifelse(P_medio >= .65, "2. Medio" ,
#                                  ifelse(P_joven >= .65,  "3. Joven",
#                                         ifelse(P_alto >= .5, "4. Alto",
#                                                ifelse((P_basico + P_medio + P_joven >= .65) & (P_basico > .2 & P_medio > .2 & P_joven > .2) , "5. Basico  - Medio - Joven",
#                                                       ifelse((P_basico + P_medio + P_alto >= .65) & (P_basico > .2 & P_medio > .2 & P_alto > .2) , "6. Basico - Medio - Alto",
#                                                              ifelse((P_medio + P_joven + P_alto >= .65) & (P_medio > .2 & P_joven > .2 & P_alto > .2) , "7. Medio - Joven - Alto",
#                                                                     ifelse((P_basico + P_medio > .65) & (P_basico > .25 & P_medio > .25) , "8. Basico - Medio",
#                                                                            ifelse((P_basico + P_joven > .65) & (P_basico > .2 & P_joven > .2) , "9. Basico - Joven",
#                                                                                   ifelse((P_basico + P_alto > .65) & (P_basico > .25 & P_alto > .25) , "10. Basico - Alto",
#                                                                                          ifelse((P_medio + P_joven > .65) & (P_medio > .25 & P_joven > .25) , "11. Medio - Joven",
#                                                                                                 ifelse((P_medio + P_alto > .65) & (P_medio > .25 & P_alto > .25) , "11. Medio - Alto",
#                                                                                                        ifelse((P_joven + P_alto > .65) & (P_joven > .25 & P_alto > .25) , "13. Joven - Alto",NA
#                                                                                                               ))))))))))))))

# empresas_fin <- empresas_fin %>% 
#   mutate(cluster_def = ifelse(P_basico+P_medio+P_joven+P_alto < 0.1, NA,
#                               ifelse(is.na(cluster),colnames(empresas_fin[4:7])[apply(empresas_fin[4:7],1,which.max)],cluster)))
# sum(is.na(empresas_fin$cluster_def))
# names(table(empresas_fin$Nomb.Empresas.Pincipal))

# Se aplica la función de limpieza y tokenización a cada comentario 
empresas_text <- empresas %>% mutate(Cluster_tokenizado = map(.x = cluster, .f = limpiar_tokenizar)) 
empresas_text %>% select(Cluster_tokenizado) %>% head()
empresas_text %>% slice(1) %>% select(Cluster_tokenizado) %>% pull()
str(empresas_text)

# Proceso de expansion
empresas_tidy <- empresas_text %>% 
  unnest() %>% 
  mutate(Cluster_tokenizado = tolower(Cluster_tokenizado)) %>% 
  mutate(Cluster_tokenizado = chartr("áéíóú","aeiou",Cluster_tokenizado)) %>% 
  mutate(Cluster_tokenizado = ifelse(Cluster_tokenizado == "media","medio",Cluster_tokenizado))
str(empresas_tidy)

listado_con <- read_excel("Listado_productos.xlsx", sheet = "Convenios") %>% 
  data.frame() %>% 
  select(NOMBRE.COMERCIAL,CATEGORIA,BASICO:ESPECIALIZADA) %>% 
  gather(key = "CLUSTER", value = "ind", 3:9) %>% 
  arrange(NOMBRE.COMERCIAL) %>% 
  filter(!is.na(ind)) %>% 
  mutate(CLUSTER = tolower(CLUSTER)) %>% 
  mutate(CLUSTER = chartr("áéíóú","aeiou",CLUSTER)) %>% 
  select(-c(ind))
str(listado_con)

listado_porta <- read_excel("Listado_productos.xlsx", sheet = "Portafolio") %>% 
  data.frame() %>% 
  select(SERVICIO,PRODUCTO,SUBPRODUCTO,BASICO:ESPECIALIZADA) %>%
  gather(key = "CLUSTER", value = "ind", 4:10) %>% 
  arrange(SERVICIO) %>% 
  filter(ind == 1) %>% 
  mutate(CLUSTER = tolower(CLUSTER)) %>% 
  mutate(CLUSTER = chartr("áéíóú","aeiou",CLUSTER)) %>% 
  select(-c(ind))
str(listado_porta)

# bd_fin <- empresas_tidy %>% 
#   left_join(listado_con, by = c("Cluster_tokenizado"="CLUSTER")) %>% 
#   left_join(listado_porta, by = c("Cluster_tokenizado"="CLUSTER")) %>% 
#   # na.omit() %>% 
#   distinct()
# str(bd_fin)

bd_fin1 <- empresas_tidy %>% 
  select(idnitprincipal,nombre_empresa_principal,cluster,Cluster_tokenizado) %>% 
  left_join(listado_con, by = c("Cluster_tokenizado"="CLUSTER")) %>%
  distinct()
str(bd_fin1)


bd_fin2 <- empresas_tidy %>% 
  select(idnitprincipal,nombre_empresa_principal,cluster,Cluster_tokenizado) %>% 
  left_join(listado_porta, by = c("Cluster_tokenizado"="CLUSTER")) %>%
  distinct()
str(bd_fin2)

write.csv2(bd_fin1, file = "bd_fin1.csv", row.names = F)
write.csv2(bd_fin2, file = "bd_fin2.csv", row.names = F)

# empresas2 <- read_excel("Base_empresas.xlsx", skip = 1) %>% data.frame() %>% 
#   select(Id.Emp.Filial,Piramide.1,Sector.Comercial,Sector.Comercial.22.Actividad,DescripcionCIIU) %>% 
#   filter(Piramide.1 %in% "1 Emp Grandes") %>% 
#   distinct() %>% 
#   select(-Piramide.1)
# str(empresas2)
# 
# bd_entrega <- bd_fin %>% 
#   left_join(empresas2, by = c("Id.Nit.Principal"="Id.Emp.Filial")) %>% 
#   select(1:5,8:10,6,7) %>% 
#   distinct()
# write.csv2(bd_fin, file = "bd_fin.csv", row.names = F)
# length(names(table(bd_fin$idnitprincipal)))
str(bd_fin1)
