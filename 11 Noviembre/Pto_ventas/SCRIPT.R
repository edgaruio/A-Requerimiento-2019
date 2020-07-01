# Cargamos librerias
rm(list = ls())
options(scipen = 999)
library(readxl); library(dplyr); library(data.table); library(tidyr)
dir()

cuadro_ingresos <- read_excel("Cuadro ingresos (planeacion).xlsx", range = "B4:E19") %>% 
  rename("UES"="UES (cifras en $mm)",
         "UES_2019"="UE 2019",
         "PPTO_2020"="PPTO 2020")
str(cuadro_ingresos)

consolidado <- read_excel("Consolidado_PPTO_Ventas_2020_UES.xlsx", sheet = "PTO_EDIT") %>% 
  mutate_at(.vars = c("ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"), .funs = as.numeric) %>% 
# %>% 
#   mutate_at(.vars = c("ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"), .funs = round)
  select(-c("ENE","FEB","MAR","ABR","MAY","JUN","JUL","AGO","SEP","OCT","NOV","DIC"))
str(consolidado)

parti_segm <- consolidado %>% 
  group_by(UES,Segmento) %>% 
  summarise(total = sum(Total)) %>% 
  mutate(p_total = total/sum(total)) %>% 
  data.frame() %>% 
  select(-total) %>% 
  spread(key = Segmento, value = p_total, fill = 0) %>% 
  data.frame()
str(parti_segm)

parti_segm_total <- consolidado %>% 
  group_by(UES) %>% 
  summarise(total = sum(ifelse(UES != "Vivienda", Total/1000000, Total))) %>% 
  # mutate(p_total = round(total/sum(total),2)) %>% 
  # data.frame() %>% 
  # select(-total) %>% 
  # spread(key = Segmento, value = total, fill = 0) %>% 
  data.frame()
str(parti_segm_total)

parti_canal <- consolidado %>% 
  mutate(Canal = tolower(Canal)) %>% 
  group_by(UES,Canal) %>% 
  summarise(total = sum(Total)) %>% 
  mutate(p_total = total/sum(total)) %>% 
  data.frame() %>% 
  select(-total) %>% 
  spread(key = Canal, value = p_total, fill = 0) %>% 
  data.frame()
str(parti_canal)

cuadro_ingresos_parti <- cuadro_ingresos %>% 
  left_join(parti_segm_total, by = c("UES_edit"="UES")) %>% 
  left_join(parti_segm, by = c("UES_edit"="UES")) %>% 
  mutate(Convenios_p = PPTO_2020*Convenios,
         Empresarial_p = PPTO_2020*Empresarial,
         Individual_p = PPTO_2020*Individual)
str(cuadro_ingresos_parti)

# Salidas
# salida_total
library(writexl)
writexl::write_xlsx(cuadro_ingresos_parti %>% 
                      select(UES:total), 
                    "Salidas/total.xlsx")
# test1 <- cuadro_ingresos_parti %>% 
#   select(Empresarial_p) %>% 
#   summarise(Empresarial_p = sum(Empresarial_p)/1000000,
#             Individual_p = sum(Empresarial_p)/1000000) %>% 
#   data.frame()
# test1
# test2 <- cuadro_ingresos_parti %>% 
#   select(PPTO_2020,Empresarial=Empresarial_p,Individual=Individual_p) %>% 
#   na.omit() %>% 
#   gather(key = "Segmento", value = "Presupuesto", 2:3) %>% 
#   group_by(Segmento) %>% 
#   summarise(Presupuesto = sum(Presupuesto, na.rm = T)) %>% 
#   data.frame()
# test2
# test <- test1 %>% 
#   left_join(test2)
writexl::write_xlsx(cuadro_ingresos_parti,
                    "Salidas/x_segmento.xlsx")




