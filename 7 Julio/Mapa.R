# Librerias
rm(list = ls())
options(digits = 10)
library(dplyr); library(leaflet); library(leaflet.extras); library(sp); library(data.table); library(RODBC); library(rgeos)
library(rgdal)

personas <- readRDS("ConsolidacionOCT2019.rds") %>% 
  data.frame() %>% 
  select(id_persona:marca_afiliado_unico) %>% 
  distinct() %>% 
  filter(!is.na(cx_persona))
str(personas)

empresas <- readRDS("ConsolidacionOCT2019.rds") %>%
  select(id_empresa:Num_cesantias) %>% 
  distinct() %>% 
  filter(!is.na(cx_empresa))
str(empresas)

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 17))
map %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addCircleMarkers(data = empresas, lng =~ cx_empresa, lat =~ cy_empresa,fillOpacity = 1, radius=3, stroke=FALSE) %>%
  # addLegend(pal=paletaafil1(), values= data_empresas()$n_empleados, opacity=0.7, title = "Numero de Empleados", position = "bottomright") %>%
  # addPolygons(data=poligonos, color = "teal", opacity = 0.1) %>% 
  addMiniMap(tiles = providers$CartoDB.Positron,toggleDisplay = TRUE)



# Creamos poligonos
cua_can <- matrix(c(4.647670, -74.099080, 4.639937, -74.094317, 4.646663, -74.086305, 4.651681, -74.093216, 4.647670, -74.099080),
                  ncol = 2, byrow = T) 
cua_can <- cbind(cua_can[,2],cua_can[,1])
P1 <- Polygon(cua_can)

cua_gran_est <- matrix(c(4.653254, -74.103781, 4.645125, -74.110565, 4.635513, -74.100811, 4.639614, -74.094929, 4.653254, -74.103781),
                       ncol = 2, byrow = T) 
cua_gran_est <- cbind(cua_gran_est[,2],cua_gran_est[,1])
P2 <- Polygon(cua_gran_est)

cua_eltiempo <- matrix(c(4.654073, -74.103265, 4.658641, -74.099512, 4.665079, -74.106946, 4.663085, -74.109081, 4.654073, -74.103265), 
                       ncol = 2, byrow = T) 
cua_eltiempo <- cbind(cua_eltiempo[,2],cua_eltiempo[,1])
P3 <- Polygon(cua_eltiempo)

cua_salitre <- matrix(c(4.666168, -74.111902, 4.656071, -74.120706, 4.645033, -74.110524, 4.653299, -74.103686, 4.666168, -74.111902), 
                      ncol = 2, byrow = T)
cua_salitre <- cbind(cua_salitre[,2],cua_salitre[,1])
P4 <- Polygon(cua_salitre)

cua_dian <- matrix(c(4.679352, -74.119317, 4.666739, -74.111336, 4.671367, -74.107011, 4.682452, -74.116054, 4.679352, -74.119317), 
                   ncol = 2, byrow = T) 
cua_dian <- cbind(cua_dian[,2],cua_dian[,1])
P5 <- Polygon(cua_dian)

cua_dorado <- matrix(c(4.678686, -74.119954, 4.672063, -74.125295, 4.661628, -74.115948, 4.666256, -74.111882, 4.678686, -74.119954), 
                     ncol = 2, byrow = T) 
cua_dorado <- cbind(cua_dorado[,2],cua_dorado[,1])
P6 <- Polygon(cua_dorado)

cua_conecta <- matrix(c(4.685237, -74.124709, 4.679248, -74.119295, 4.681655, -74.116749, 4.687045, -74.120611, 4.685237, -74.124709), 
                      ncol = 2, byrow = T) 
cua_conecta <- cbind(cua_conecta[,2],cua_conecta[,1])
P7 <- Polygon(cua_conecta)

cua_patios_tras <- matrix(c(4.684473, -74.124865, 4.677971, -74.130423, 4.672197, -74.125358, 4.678698, -74.119908, 4.684473, -74.124865), 
                          ncol = 2, byrow = T)
cua_patios_tras <- cbind(cua_patios_tras[,2],cua_patios_tras[,1])
P8 <- Polygon(cua_patios_tras)

P1s <- Polygons(list(P1), 1)
P2s <- Polygons(list(P2), 2)
P3s <- Polygons(list(P3), 3)
P4s <- Polygons(list(P4), 4)
P5s <- Polygons(list(P5), 5)
P6s <- Polygons(list(P6), 6)
P7s <- Polygons(list(P7), 7)
P8s <- Polygons(list(P8), 8)

SPlgs <- SpatialPolygons(list(P1s, P2s, P3s, P4s, P5s, P6s, P7s, P8s))

df <- data.frame(ID = 1:8)

SPDF <- SpatialPolygonsDataFrame(SPlgs, df)
saveRDS(SPDF, file = "Capa_poligonos.rds")

plot(SPDF, axes=T, col = 'blue')
class(SPDF)

### EMPRESAS ====
#Interseccion de puntos en Poligonos Empresas lat and long
Lat <- empresas$cx_empresa #c(4.646165, 4.645029)
Lon <- empresas$cy_empresa #c(-74.09475, -74.102701)
#make a data frame
coords <- data.frame(cbind(Lon,Lat)) %>% 
  na.omit()
str(coords)
#and into Spatial
points <- SpatialPoints(coords)
class(points)
#assume same proj as shapefile!
proj4string(points) <- proj4string(SPDF)
#get county polygon point is in
result <- as.data.frame(over(points, SPDF))
dim(result)
sum(is.na(result$ID))

geo_empresas <- empresas %>% 
  mutate(id_interes = result$ID) %>% 
  filter(!is.na(id_interes)) %>% 
  mutate(id_poligono = ifelse(id_interes == 1, "CAN",
                             ifelse(id_interes == 2, "GRAN ESTACION",
                                    ifelse(id_interes == 3, "EL TIEMPO",
                                           ifelse(id_interes == 4, "SALITRE",
                                                  ifelse(id_interes == 5, "DIAN",
                                                         ifelse(id_interes == 6, "DORADO",
                                                                ifelse(id_interes == 7, "CONECTA","PATIOS TRANS"))))))))
str(geo_empresas)
saveRDS(geo_empresas, file = "geo_empresas.rds")

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addMarkers(data = geo_empresas, lng =~ CX, lat =~ CY) %>%
  addPolygons(data=SPDF, color = "navy") 


### PERSONAS ====
# Interseccion personas en poligonos lat and long
Lat_v <- personas$cy_persona #c(4.646165, 4.645029)
Lon_v <- personas$cx_persona #c(-74.09475, -74.102701)
Lat_t <- personas$cy_empresa #c(4.646165, 4.645029)
Lon_t <- personas$cx_empresa #c(-74.09475, -74.102701)

#make a data frame
coords_v <- as.data.frame(cbind(Lon_v,Lat_v)) %>% na.omit()
coords_t <- as.data.frame(cbind(Lon_t,Lat_t)) %>% na.omit()
#and into Spatial
points_v <- SpatialPoints(coords_v)
points_t <- SpatialPoints(coords_t)
#assume same proj as shapefile!
proj4string(points_v) <- proj4string(SPDF)
proj4string(points_t) <- proj4string(SPDF)
#get county polygon point is in
result_v <- as.data.frame(over(points_v, SPDF))
result_t <- as.data.frame(over(points_t, SPDF))
dim(result)
sum(!is.na(result$ID))

geo_personas <- personas %>% 
  mutate(id_interes_v = result_v$ID,
         id_interes_t = result_t$ID) %>% 
  filter(!is.na(id_interes_v) | !is.na(id_interes_t)) %>% 
  mutate(id_poligono_v = ifelse(id_interes_v == 1, "CAN",
                              ifelse(id_interes_v == 2, "GRAN ESTACION",
                                     ifelse(id_interes_v == 3, "EL TIEMPO",
                                            ifelse(id_interes_v == 4, "SALITRE",
                                                   ifelse(id_interes_v == 5, "DIAN",
                                                          ifelse(id_interes_v == 6, "DORADO",
                                                                 ifelse(id_interes_v == 7, "CONECTA","PATIOS TRAS"))))))),
         id_poligono_t = ifelse(id_interes_t == 1, "CAN",
                                ifelse(id_interes_t == 2, "GRAN ESTACION",
                                       ifelse(id_interes_t == 3, "EL TIEMPO",
                                              ifelse(id_interes_t == 4, "SALITRE",
                                                     ifelse(id_interes_t == 5, "DIAN",
                                                            ifelse(id_interes_t == 6, "DORADO",
                                                                   ifelse(id_interes_t == 7, "CONECTA","PATIOS TRAS"))))))))
dim(geo_personas)
saveRDS(geo_personas, file = "geo_personas.rds")

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addCircles(data = geo_personas, lng =~ cx_persona, lat =~ cy_persona, opacity = 0.09, color = "darkgreen", radius = 0.001) %>%
  addPolygons(data=SPDF, color = "navy") 





# Creamos poligonos

# map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
# map %>%
#   addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
#   addMarkers(data = geo_empresas, lng =~ CX, lat =~ CY) %>%
#   addPolygons(data=cua_can, color = "navy") #%>%
  # addPolygons(data=cundi, fill = F, stroke = T, color = "red", group = "study area") %>%
  # addLayersControl(
  #   # baseGroups  = c("1 Emp Grandes","2 Emp Medio","3 Empresas Pymes","4 Micro"),
  #   overlayGroups =  c("Centros de Servicio","Educación","Supermercados","Medicamentos","Recreación y Turismo","Salud","Vivienda"),
  #   options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>%
  # addMarkers(data=CSERVICIOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsCS, group = "Centros de Servicio") %>%
  # addMarkers(data=EDUCACION, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsED, group = "Educación") %>%
  # addMarkers(data=SUPERMERCADOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsSP, group = "Supermercados") %>%
  # addMarkers(data=MEDICAMENTOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsDR, group = "Medicamentos") %>%
  # addMarkers(data=RYT, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsRYT, group = "Recreación y Turismo") %>%
  # addMarkers(data=SALUD, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsSL, group = "Salud") %>%
  # addMarkers(data=VIVIENDA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
  #            icon = leafIconsVV, group = "Vivienda") %>%
  # hideGroup(c("Educación","Supermercados","Medicamentos","Recreación y Turismo","Salud","Vivienda")) %>%
  # # addLegend(pal=mypalette, values=~aportes_pro, opacity=0.9, title = "Aportes Promedio", position = "bottomright") %>%
  # setView(input$CX, input$CY, zoom = 15)
