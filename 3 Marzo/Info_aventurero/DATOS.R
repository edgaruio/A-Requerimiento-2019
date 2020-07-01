# Datos
library(data.table); library(dplyr); library(readxl); library(tidyr)

emp_temporales <- read_excel("Empresas_Consultas_Temporales.xlsx") %>% 
  data.frame()
str(emp_temporales)

info1 <- fread("info_p1.txt", sep = ";", dec = ",") %>% 
  data.frame()
str(info1)

info2 <- fread("info_p2.txt") %>% 
  data.frame()
str(info2)

consumo_individual <- fread("consumo_individual_edit.csv") %>% 
  data.frame()
str(consumo_individual)

# CC1019078197
consumo_ryt_emp1 <- consumo_individual %>% 
  select(id_empresa,id_persona,servicio) %>% 
  group_by(id_empresa, servicio) %>% 
  summarise(n_id_persona = n_distinct(id_persona)) %>% 
  spread(key = servicio, value = n_id_persona) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  mutate_all(funs(ifelse(is.infinite(.),0,.))) %>%
  mutate(club = `Club Bellavista` + `Club Calle 195` + `Club El Cubo` + `Club La Colina`,
         hotel = `Hotel Alcaravan` + `Hotel Bosques De Athan` + `Hotel Penalisa` + `Hoteles Paipa`)
str(consumo_ryt_emp1)

consumo_ryt_emp2 <- consumo_individual %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_empresa, ues) %>% 
  summarise(RyT = n_distinct(id_persona)) %>% 
  select(-ues)

consumo_ryt_emp <- left_join(consumo_ryt_emp1,consumo_ryt_emp2)
str(consumo_ryt_emp)
rm(consumo_ryt_emp1,consumo_ryt_emp2)

aventurero <- fread("id_eventurero.txt", sep = ";") %>% 
  filter(marca_aventurero == 1) %>% 
  group_by(id_empresa) %>% 
  summarise(n_person = n_distinct(id_persona),
            # n_per_aventu = sum(marca_aventurero),
            n_hijos_aven = sum(numero_hijos))
str(aventurero)

info <- info1 %>% 
  left_join(info2,by = "id_empresa") %>% 
  left_join(consumo_ryt_emp, by = "id_empresa") %>% 
  left_join(aventurero, by = "id_empresa") %>% 
  mutate_all(funs(ifelse(is.na(.),0,.)))
str(info)

# Cargamos bases de datos
bd_empresas <- fread("Info_emp_obj_21012019.csv", sep = ";", dec = ",") %>% 
  data.frame() %>%
  select(id_empresa,Subsidio_asignado,Consumo_credito,Compra_vivienda,Cuad_A:Cuad_B)
str(bd_empresas)  

bd_temporales <- emp_temporales %>% 
  left_join(info %>% select(id_empresa,Afiliados,Básico:Alto,RyT,n_person,n_hijos_aven,Habeas_data),
            by = c("Id.Emp.Filial"="id_empresa")) %>% 
  left_join(bd_empresas, by = c("Id.Emp.Filial"="id_empresa"))
str(bd_temporales)

### Consumos por empresa los dos mas frecuentes
consumo <- readRDS("consumo_individual.rds") %>% 
  data.frame() %>% 
  mutate(anio_mes = paste(año, mes, sep = "")) %>% 
  filter(anio_mes >= 20182)
str(consumo)

consumo_agru <- consumo %>% 
  mutate(Producto = ifelse(ues == "Credito Social", servicio, ues)) %>% 
  group_by(id_empresa,Producto) %>% 
  summarise(visita = n()) %>% 
  arrange(id_empresa,desc(visita)) %>% 
  filter(row_number() <= 2) %>%
  data.frame() %>% 
  group_by(id_empresa) %>%
  summarise(Prod = paste(Producto[1], Producto[2], sep = ", "))
str(consumo_agru)
sum(duplicated(consumo_agru$id_empresa))

bd_temporales <- bd_temporales %>% 
  left_join(consumo_agru, by =c("Id.Emp.Filial"="id_empresa")) 

write.csv2(bd_temporales, file = "bd_temporales.csv", row.names = F)
