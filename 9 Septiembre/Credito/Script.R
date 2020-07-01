library(dplyr); library(readxl); library(data.table); library(tidyr)

datos <- read_excel("Libro1.xlsx") %>% 
  data.frame() %>% 
  mutate(id_persona = paste("CC",IDENTIFICACION,sep = "")) %>% 
  select(id_persona) %>% 
  distinct()
str(datos)

consolidada <- fread("Consolidacion.csv", encoding = 'UTF-8') %>% 
  data.frame() %>% 
  select(id_persona,Genero,Categoria,Segmento_poblacional,Edad,Salario,fecha_afiliacion,fecha_retiro,id_empresa,RazonSocial,Piramide1,Piramide2,Cat_A,Cat_B,Cat_C,Seg_Alto:Seg_Medio) %>% 
  distinct() %>% 
  mutate(Segmento_poblacional = iconv(Segmento_poblacional,to="ASCII//TRANSLIT")) %>% 
  group_by(id_persona) %>% 
  arrange(desc(Salario)) %>% 
  filter(row_number()==1) %>% 
  data.frame()
str(consolidada)
table(duplicated(consolidada$id_persona))

consulta <- datos %>% 
  left_join(consolidada, by = "id_persona") %>% 
  mutate(estado = ifelse(is.na(id_empresa),"No afiliado","Afiliado")) %>% 
  select(-c(Cat_A:Seg_Medio))
str(consulta)
table(consulta$estado)
fwrite(consulta, "consulta.csv", row.names = F)

tabla1 <- consulta %>% 
  filter(estado == "Afiliado") %>% 
  group_by(Piramide1,Piramide2) %>% 
  summarise(Nits = n_distinct(id_empresa),
            Afiliados = n_distinct(id_persona))
data.frame()

tabla2 <- consulta %>% 
  filter(estado == "Afiliado") %>% 
  group_by(Piramide2,Categoria) %>% 
  summarise(Conteo = n_distinct(id_persona)) %>% 
  data.frame() %>% 
  spread(Categoria, Conteo)
str(tabla2)

tabla3 <- consulta %>% 
  filter(estado == "Afiliado") %>% 
  group_by(Piramide2,Segmento_poblacional) %>% 
  summarise(Conteo = n_distinct(id_persona)) %>% 
  data.frame()%>% 
  spread(Segmento_poblacional, Conteo)
str(tabla3)

tabla <- tabla1 %>% 
  left_join(tabla2, by = "Piramide2") %>% 
  left_join(tabla3, by = "Piramide2")
fwrite(tabla, "tabla.csv", row.names = F)
