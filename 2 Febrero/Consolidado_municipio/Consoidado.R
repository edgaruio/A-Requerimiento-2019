# Unimos informacion
rm(list = ls())
dir()

library(data.table); library(dplyr); library(readxl)

# COnsolidado info

info1 <- fread("Consolidado_Info.csv", na.strings = c("","na","NA")) %>% 
  select(IDEMP,RAZON.SOCIAL,nombre_municipio,nombre_departamento)
str(info1)

info_emp <- fread("Info_emp_obj_21012019.csv", na.strings = c("","na","NA")) %>% 
  select(id_empresa,nombre,Cuota_derecho,Cuota_redimida,Habeas_data,Afiliados)
str(info_emp)

contacta <- fread("Contactabilidad.txt") %>%
  distinct()
str(contacta)

supermercados <- read_excel("Listado_supermercados.xlsx") %>% 
  data.frame() %>% 
  group_by(CIUDAD) %>% 
  summarise(Conteo = n()) %>% 
  filter(Conteo == 1)
str(supermercados)


transa_tms <- fread("transacciones_medios_de_pago_2018.txt") %>% 
  select(no_tarjeta_multiservicios) %>% 
  mutate(no_tarjeta_multiservicios = as.character(no_tarjeta_multiservicios)) %>% 
  mutate(Tarjeta = gsub("88000100","",no_tarjeta_multiservicios)) %>% 
  distinct() %>% 
  select(-no_tarjeta_multiservicios) %>% 
  mutate(redime = "Si")
str(transa_tms)

consulta1 <- fread("Consulta1.txt", sep = ";", dec = ",") %>% 
  select(id_persona,Tarjeta) %>% 
  mutate(Tarjeta = as.character(Tarjeta))
str(consulta1)

idper_idemp <- fread("consulta_idper_idemp.txt")
str(idper_idemp)

redimio_contacta <- fread("base.txt")
str(redimio_contacta)
redimio_contacta <- left_join(redimio_contacta,info1, by = c("id_empresa"="IDEMP"))
str(redimio_contacta) 
table(redimio_contacta$redimio)
  
test1 <- redimio_contacta %>% 
  filter(redimio == "SI") %>% 
  group_by(nombre_municipio) %>% 
  summarise(conteo_redime = n_distinct(id_persona))

test2 <- redimio_contacta %>% 
  filter(redimio == "SI" & autorizacion == "SI") %>% 
  group_by(nombre_municipio) %>% 
  summarise(conteo_contacta = n_distinct(id_persona))

test <- left_join(test1, test2)

str(redimio_contacta)

info_glob <- idper_idemp %>% 
  left_join(consulta1, by = c("id_persona"="id_persona")) %>% 
  left_join(transa_tms, by = c("Tarjeta"="Tarjeta")) %>% 
  left_join(contacta, by = c("id_persona"="id_persona")) %>% 
  select(id_empresa,id_persona,redime,autorizacion) %>% 
  filter(redime == "Si") %>% 
  arrange(id_empresa) %>% 
  distinct() 
str(info_glob)

info_glob_fin <- info_glob %>% 
  left_join(info1 %>% select(IDEMP,nombre_municipio), 
            by = c("id_empresa"="IDEMP")) %>% 
  distinct() %>% 
  filter(autorizacion == "SI") %>% 
  group_by(nombre_municipio) %>% 
  summarise(n_autoriza = n_distinct(id_persona))
str(info_glob_fin)


bd_entrega <- left_join(info1,info_emp,by=c("IDEMP"="id_empresa")) %>% 
  data.frame()
str(bd_entrega)

# sort(unique(names(table(bd_entrega$nombre_municipio))))
# sort(unique(names(table(supermercados$CIUDAD))))
# unique(bd_entrega$nombre_municipio %in% supermercados$CIUDAD)

base_agre <- bd_entrega %>% 
  filter(!is.na(nombre_municipio)) %>% 
  group_by(nombre_municipio) %>% 
  summarise(n_emp = n_distinct(IDEMP),
            Cuota_derecho = sum(Cuota_derecho, na.rm = T),
            Cuota_redimida = sum(Cuota_redimida, na.rm = T),
            Habeas_data = sum(Habeas_data, na.rm = T),
            Afiliados = sum(Afiliados, na.rm = T)) %>% 
  # mutate(por_cuota_der = round(Cuota_derecho/Afiliados,3),
  #        por_cuota_red = round(Cuota_redimida/Afiliados,3),
  #        por_contactabilidad = round(Habeas_data/Afiliados,3)) %>% 
  arrange(desc(Afiliados))
View(base_agre)

base_agre_fin <- base_agre %>% 
  left_join(info_glob_fin, by = c("nombre_municipio"="nombre_municipio")) %>% 
  mutate(test = ifelse(Cuota_redimida < n_autoriza, "Si", "No")) %>% 
  filter(nombre_municipio %in% supermercados$CIUDAD) %>% 
  mutate(n_autoriza2 = ifelse(test == "Si", 0.8*n_autoriza, n_autoriza))
str(base_agre_fin)

entrega <- left_join(base_agre_fin,test)


write.csv2(entrega, file = "entrega.csv", row.names = F)
