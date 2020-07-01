#### Consulta Falabella ====
library(dplyr); library(data.table); library(tidyr)
bd_empresas <- readRDS(file = "Info_emp_act_23102019.rds")
str(bd_empresas)

test <- bd_empresas %>% 
  filter(n_empleados >= 250 & n_empleados <= 550,
         ACTIVIDAD == "Prestación de servicios financieros") 
summary(test)

consulta_Seg_Emp <- fread("Consulta_Seg_Emp.txt") %>% 
  mutate(ID_CIIU = as.character(ID_CIIU)) %>% 
  # filter(ID_CIIU %in% c("6412","4719","6621","6593","7911")) %>% 
  select(id_empresa,razon_social,id_persona,ID_CIIU,categoria) %>% 
  group_by(id_empresa,razon_social,ID_CIIU,categoria) %>% 
  summarise(n_emp = n()) %>% 
  spread(categoria,n_emp) %>% 
  data.frame() %>% 
  mutate(n_emp = rowSums(.[,c("A","B","C")],na.rm = T)) %>% 
  filter(id_empresa %in% c("NIT8903002794","NIT8909009431","NIT8600692652","NIT8903084582","NIT8000786924"))
str(consulta_Seg_Emp)


consumo_ind <- readRDS("consumo_individual_julio.rds") %>% 
  filter(id_empresa %in% c("NIT8903002794","NIT8909009431","NIT8600692652","NIT8903084582","NIT8000786924"),
         anno == 2019) %>% 
  mutate(servicio2 = ifelse(servicio != "Supermercados" & servicio != "Medicamentos", "otros", servicio),
         llave = paste(ues,servicio2,sep="_")) %>% 
  group_by(id_empresa,llave) %>% 
  summarise(consumo = sum(consumo, na.rm = T)) %>% 
  spread(llave,consumo,fill = 0) %>% 
  data.frame()
str(consumo_ind)
fwrite(consumo_ind,"Consumos_individuales.csv", sep = ";", dec = ",")
table(consumo_ind$servicio)

consumo_emp <- readRDS("consumo_empresarial_julio.rds") %>% 
  filter(id_empresa %in% c("NIT8903002794","NIT8909009431","NIT8600692652","NIT8903084582","NIT8000786924"),
         anno == 2019) %>% 
  mutate(servicio2 = ifelse(servicio != "Supermercados" & servicio != "Medicamentos", "otros", servicio),
         llave = paste(ues,servicio2,sep="_")) %>% 
  group_by(id_empresa,llave) %>% 
  summarise(consumo = sum(consumo, na.rm = T)) %>% 
  spread(llave,consumo,fill = 0) %>% 
  data.frame()
str(consumo_emp)
fwrite(consumo_emp,"Consumos_empresariales.csv", sep = ";", dec = ",")
