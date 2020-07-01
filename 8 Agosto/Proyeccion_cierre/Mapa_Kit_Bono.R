# Conectamos a access - Pronosticos para: 
# Cuota monetaria girada, kit escolar, Bono lonchera & otros subsidios
rm(list = ls())
options(scipen = 999)
library(RODBC); library(data.table); library(dplyr); library(tidyr); library(forecast); library(ggplot2); library(readxl)

# Consolidada
consolidada <- readRDS("Consolidacion.rds") %>% 
  select(id_persona,Genero,Categoria,Salario,Edad,RangoEdad,ActividadCIIU) %>% 
  distinct() %>% 
  na.omit() %>% 
  group_by(id_persona) %>% 
  arrange(desc(Salario)) %>% 
  filter(row_number()==1)
table(duplicated(consolidada$id_persona))
str(consolidada)

# Infraestructura
geo_supermercados <- read_excel("Supermercados_Geo.xlsx") %>% 
  data.frame()
str(geo_supermercados)

# Bono Lonchera
conn_bono_lonchera <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/AGOSTO/Proyeccion Cierre 2019/BonoLonchera.accdb")
subset(sqlTables(conn_bono_lonchera), tableType = "SYSTEM TABLE")
consulta_bono <- sqlFetch(conn_bono_lonchera, "BonoLochera")
str(consulta_bono)
odbcClose(conn_bono_lonchera)

df_bono <- consulta_bono %>% 
  mutate(anio_mes = paste0(AÑO,MES)) %>% 
  filter(anio_mes %in% c(20196)) %>% 
  mutate(id_persona = as.character(id_persona),
         Fecha_derecho = paste(AÑO,MES,sep="_"),
         Fecha_redime = paste(AÑO_REDENCION,MES_REDENCION,sep="_"),
         REDIMIO2 = ifelse(is.na(REDIMIO),"No","Si")) %>% 
  select(id_persona,Fecha_derecho,Fecha_redime,REDIMIO,REDIMIO2) %>% 
  distinct()
str(df_bono)
table(df_bono$anio_mes)
table(df_bono$REDIMIO)
table(duplicated(df_bono$id_persona))

df_bono_geo <- df_bono %>% 
  left_join(consolidada, by = "id_persona")
str(df_bono_geo)
sum(is.na(df_bono_geo$cx_persona))

# Mapa e Infraestrutura
library(leaflet); library(rgdal)

## Capas
cundi <- readRDS("poligonos-localidades/Cundinamarca.rds")
localidad <- readOGR("poligonos-localidades/poligonos-localidades.shp")

leafIconsSUP <- icons(
  iconUrl = "icons/ICONOS_ACT/Supermercados.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 16, iconAnchorY = 40)

# Si redime
df_bono_geo_si <- df_bono_geo %>% filter(REDIMIO2 == "Si") %>% filter(!is.na(cx_persona))
map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(data=localidad, fill = F, stroke = T, color = "teal", group = "study area") %>%
  addPolygons(data=cundi, fill = F, stroke = T, color = "darkseagreen1", group = "study area") %>%
  addCircleMarkers(data=df_bono_geo_si, lng =~cx_persona, lat =~cy_persona,fillOpacity = 0.1,radius = 1, stroke=FALSE) %>%
  addLayersControl(
    # baseGroups  = c("1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro"),
    overlayGroups =  c("Supermercados"),
    options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>%
  addMarkers(data=geo_supermercados, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
             icon = leafIconsSUP, group = "Supermercados") %>%
  hideGroup(c("Supermercados")) %>%
  setView(-74.07739,	4.622253, zoom = 10)

# No redime
df_bono_geo_no <- df_bono_geo %>% filter(REDIMIO2 == "No") %>% filter(!is.na(cx_persona))
map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(data=localidad, fill = F, stroke = T, color = "teal", group = "study area") %>%
  addPolygons(data=cundi, fill = F, stroke = T, color = "darkseagreen1", group = "study area") %>%
  addCircleMarkers(data=df_bono_geo_no, lng =~cx_persona, lat =~cy_persona,fillOpacity = 0.1,radius = 1, stroke=FALSE) %>%
  addLayersControl(
    # baseGroups  = c("1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro"),
    overlayGroups =  c("Supermercados"),
    options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>%
  addMarkers(data=geo_supermercados, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
             icon = leafIconsSUP, group = "Supermercados") %>%
  hideGroup(c("Supermercados")) %>%
  setView(-74.07739,	4.622253, zoom = 10)


# Kit escolar 2019
conn_kit2019 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/AGOSTO/Proyeccion Cierre 2019/Kit_Escolar_2019.accdb")
subset(sqlTables(conn_kit2019), tableType = "SYSTEM TABLE")
kit2019 <- sqlFetch(conn_kit2019, "base_kit_escolar_2019")
str(kit2019)
odbcClose(conn_kit2019)
table(kit2019$estado_bono_escolar)

df_kit <- kit2019 %>% 
  select(id_persona,estado_kit_escolar,estado_bono_escolar) %>% 
  mutate(id_persona = as.character(id_persona)) %>% 
  distinct() %>% 
  data.frame()
str(df_kit)
table(df_kit$estado_kit_escolar)

df_kit_ini <- df_kit %>% 
  # left_join(consolidada, by = c("id_persona")) %>% 
  # filter(!is.na(cx_persona)) %>% 
  data.frame() %>% 
  select(id_persona,estado_bono_escolar) %>% 
  distinct()
str(df_kit_ini)
sum(is.na(df_kit_geo))

# Redime Kit
df_kit_geo_si <- df_kit_geo %>% filter(estado_kit_escolar == "ENTREGADO")
map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(data=localidad, fill = F, stroke = T, color = "teal", group = "study area") %>%
  addPolygons(data=cundi, fill = F, stroke = T, color = "darkseagreen1", group = "study area") %>%
  addCircleMarkers(data=df_kit_geo_si, lng =~cx_persona, lat =~cy_persona,fillOpacity = 0.1,radius = 1, stroke=FALSE) %>%
  addLayersControl(
    # baseGroups  = c("1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro"),
    overlayGroups =  c("Supermercados"),
    options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>%
  addMarkers(data=geo_supermercados, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
             icon = leafIconsSUP, group = "Supermercados") %>%
  hideGroup(c("Supermercados")) %>%
  setView(-74.07739,	4.622253, zoom = 10)

# No Redime Kit
df_kit_geo_no <- df_kit_geo %>% filter(estado_kit_escolar == "DISPONIBLE")
map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(data=localidad, fill = F, stroke = T, color = "teal", group = "study area") %>%
  addPolygons(data=cundi, fill = F, stroke = T, color = "darkseagreen1", group = "study area") %>%
  addCircleMarkers(data=df_kit_geo_no, lng =~cx_persona, lat =~cy_persona,fillOpacity = 0.1,radius = 1, stroke=FALSE) %>%
  addLayersControl(
    # baseGroups  = c("1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro"),
    overlayGroups =  c("Supermercados"),
    options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>%
  addMarkers(data=geo_supermercados, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
             icon = leafIconsSUP, group = "Supermercados") %>%
  hideGroup(c("Supermercados")) %>%
  setView(-74.07739,	4.622253, zoom = 10)

# Calculamos el supermercado mas cercano
# Matiz viven
library(geosphere)
bd_matriz_v <- data.frame(distm(df_bono_geo[,c('cx_persona','cy_persona')], geo_supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_v)

# Matriz trabajan
bd_matriz_t <- data.frame(distm(df_bono_geo[,c('cx_empresa','cy_empresa')], geo_supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_t)

# Calculo distancias
mat_v <- bd_matriz_v
df_bono_geo$dis_v <- round(apply(mat_v[1:dim(mat_v)[2]],1,min),2)
df_bono_geo$point_v <- geo_supermercados$NOMBRE[max.col(-mat_v)]

mat_t <- bd_matriz_t
df_bono_geo$dis_t <- round(apply(mat_t[1:dim(mat_t)[2]],1,min),2)
df_bono_geo$point_t <- geo_supermercados$NOMBRE[max.col(-mat_t)]

# Para kit escolar
bd_matriz_v2 <- data.frame(distm(df_kit_geo[,c('cx_persona','cy_persona')], geo_supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_v2)

# Matriz trabajan
bd_matriz_t2 <- data.frame(distm(df_kit_geo[,c('cx_empresa','cy_empresa')], geo_supermercados[,c('CX','CY')], fun=distVincentyEllipsoid)/1000)
str(bd_matriz_t2)

# Calculo distancias
mat_v2 <- bd_matriz_v2
df_kit_geo$dis_v <- round(apply(mat_v2[1:dim(mat_v2)[2]],1,min),2)
df_kit_geo$point_v <- geo_supermercados$NOMBRE[max.col(-mat_v2)]

mat_t2 <- bd_matriz_t2
df_kit_geo$dis_t <- round(apply(mat_t2[1:dim(mat_t2)[2]],1,min),2)
df_kit_geo$point_t <- geo_supermercados$NOMBRE[max.col(-mat_t2)]

# Escribir base de datos
fwrite(df_bono_geo, "df_bono_geo.csv", row.names = F)
fwrite(df_kit_geo, "df_kit_geo.csv", row.names = F)

# Graficos
library(dplyr); library(data.table); library(esquisse); library(ggplot2)
df_bono_geo <- fread("df_bono_geo.csv") %>% 
  data.frame() %>% 
  na.omit() %>% 
  left_join(consolidada, by = "id_persona")
str(df_bono_geo)

df_kit_geo <- fread("df_kit_geo.csv") %>% 
  data.frame() %>% 
  na.omit() %>% 
  left_join(consolidada, by = "id_persona") %>% 
  left_join(df_kit_ini, by = "id_persona") %>% 
  mutate(estado_bono_escolar = ifelse(estado_bono_escolar == "ENTREGADO", "ENTREGADO", "DISPONIBLE"))
str(df_kit_geo)
# esquisse::esquisser()

df_bono_geo <- df_bono_geo %>%
 filter(dist_vt >= 0L & dist_vt <= 10L) %>%
 filter(!(Categoria %in% 
    "C"))

library(ggplot2)

ggplot(df_bono_geo) +
 aes(x = REDIMIO2, y = dist_vt, fill = REDIMIO2) +
 geom_boxplot() +
 scale_fill_brewer(palette = "Dark2") +
 labs(x = "Redime", y = "Distancia (Km)", title = "Distribución Distancias ", subtitle = "Redención Bono Lonchera por Actividad", fill = "Redime") +
 theme_minimal() +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(vars(ActividadCIIU)) + coord_flip()     

# # Ajustes distancias
# df_bono_geo <- fread("df_bono_geo.csv") %>% 
#   data.frame() %>% 
#   mutate(dist_vt = ifelse(dis_v < dis_t, dis_v, dis_t),
#          point_vt = ifelse(dis_v < dis_t, point_v, point_t))
# str(df_bono_geo)
# df_kit_geo <- fread("df_kit_geo.csv") %>% 
#   data.frame() %>% 
#   mutate(dist_vt = ifelse(dis_v < dis_t, dis_v, dis_t),
#          point_vt = ifelse(dis_v < dis_t, point_v, point_t))
# str(df_kit_geo)


df_kit_geo <- df_kit_geo %>%
  filter(dist_vt >= 0L & dist_vt <= 10L) %>%
  filter(!(Categoria %in% 
             "C")) %>% 
  na.omit()

library(ggplot2)

ggplot(df_kit_geo) +
  aes(x = estado_kit_escolar, y = dist_vt, fill = estado_kit_escolar) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Redime", y = "Distancia (Km)", title = "Distribución Distancias ", subtitle = "Redención Kit Escolar por Actividad", fill = "Redime") +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(vars(ActividadCIIU)) + coord_flip()     

ggplot(df_kit_geo) +
  aes(x = estado_bono_escolar, y = dist_vt, fill = estado_bono_escolar) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Redime", y = "Distancia (Km)", title = "Distribución Distancias ", subtitle = "Redención Bono Kit Escolar por Actividad", fill = "Redime") +
  theme_minimal() +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(vars(ActividadCIIU)) + coord_flip()     



# fwrite(df_bono_geo, "df_bono_geo2.csv", row.names = F)
# fwrite(df_kit_geo, "df_kit_geo2.csv", row.names = F)


