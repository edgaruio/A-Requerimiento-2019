# Test
rm(list = ls())
library(sp); library(mapview)
library(dplyr); library(leaflet); library(leaflet.extras); library(sp); library(data.table); library(RODBC); library(rgeos)
library(rgdal)

personas <- readRDS("ConsolidacionOCT2019.rds") %>% 
  data.frame() %>% 
  dplyr::select(id_persona:marca_afiliado_unico) %>% 
  distinct() %>% 
  filter(!is.na(cx_persona))
str(personas)

empresas <- readRDS("ConsolidacionOCT2019.rds") %>%
  dplyr::select(id_empresa:Num_cesantias) %>% 
  distinct() %>% 
  filter(!is.na(cx_empresa))
str(empresas)


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

plot(SPDF, axes=T, col = 'blue')

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addMarkers(data = geo_empresas, lng =~ CX, lat =~ CY) %>%
  addPolygons(data=Ps1, color = "navy") #%>%





WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
mb_spat <-  SpatialPointsDataFrame(coords = cua_can[,c("CX", "CY")], 
                                   data = cua_can, 
                                   proj4string = WGS84)
spplot(mb_spat)

# #Interseccion de puntos en Poligonos
# library(sp)
# library(rgdal)
# #lat and long
# Lat <- 4.6440314
# Lon <- -74.0945276
# #make a data frame
# coords <- as.data.frame(cbind(Lon,Lat))
# #and into Spatial
# points <- SpatialPoints(coords)
# #SpatialPolygonDataFrame - I'm using a shapefile of UK counties
# counties <- readRDS("localidad.rds")
# #assume same proj as shapefile!
# proj4string(points) <- proj4string(counties)
# #get county polygon point is in
# result <- as.character(over(points, counties)$Nombre_de_l)
# 
# 


#### Version 2 =====
# Creamos poligonos
cua_can <- matrix(c(4.647670, -74.099080, 4.639937, -74.094317, 4.646663, -74.086305, 4.651681, -74.093216, 4.647670, -74.099080),
                  ncol = 2, byrow = T) 
cua_can <- cbind(cua_can[,2],cua_can[,1])

cua_gran_est <- matrix(c(4.653254, -74.103781, 4.645125, -74.110565, 4.635513, -74.100811, 4.639614, -74.094929, 4.653254, -74.103781),
                       ncol = 2, byrow = T) 
cua_gran_est <- cbind(cua_gran_est[,2],cua_gran_est[,1])

cua_eltiempo <- matrix(c(4.654073, -74.103265, 4.658641, -74.099512, 4.665079, -74.106946, 4.663085, -74.109081, 4.654073, -74.103265), 
                       ncol = 2, byrow = T) 
cua_eltiempo <- cbind(cua_eltiempo[,2],cua_eltiempo[,1])

cua_salitre <- matrix(c(4.666168, -74.111902, 4.656071, -74.120706, 4.645033, -74.110524, 4.653299, -74.103686, 4.666168, -74.111902), 
                      ncol = 2, byrow = T)
cua_salitre <- cbind(cua_salitre[,2],cua_salitre[,1])

cua_dian <- matrix(c(4.679352, -74.119317, 4.666739, -74.111336, 4.671367, -74.107011, 4.682452, -74.116054, 4.679352, -74.119317), 
                   ncol = 2, byrow = T) 
cua_dian <- cbind(cua_dian[,2],cua_dian[,1])

cua_dorado <- matrix(c(4.678686, -74.119954, 4.672063, -74.125295, 4.661628, -74.115948, 4.666256, -74.111882, 4.678686, -74.119954), 
                     ncol = 2, byrow = T) 
cua_dorado <- cbind(cua_dorado[,2],cua_dorado[,1])

cua_conecta <- matrix(c(4.685237, -74.124709, 4.679248, -74.119295, 4.681655, -74.116749, 4.687045, -74.120611, 4.685237, -74.124709), 
                      ncol = 2, byrow = T) 
cua_conecta <- cbind(cua_conecta[,2],cua_conecta[,1])

cua_patios_tras <- matrix(c(4.684473, -74.124865, 4.677971, -74.130423, 4.672197, -74.125358, 4.678698, -74.119908, 4.684473, -74.124865), 
                          ncol = 2, byrow = T)
cua_patios_tras <- cbind(cua_patios_tras[,2],cua_patios_tras[,1])

# create a polygon
a_poly = st_polygon(list(rbind(c(-74.099080,4.647670), c(-74.094317,4.639937), c(-74.086305,4.646663), c(-74.093216,4.651681), c(-74.099080,4.647670))))
a = st_sfc(a_poly)
# create points
p_matrix = as.matrix(coords)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")

res <- st_intersects(p, a, sparse = FALSE) %>% 
  data.frame() %>% 
  rename(x1=.) %>% 
  filter(x1 == TRUE)
sum(is.na(res$.))


# Version 3 closed polygon:
inter1 <- point.in.polygon(empresas$cx_empresa,
                           empresas$cy_empresa,
                           c(-74.099080, -74.094317, -74.086305, -74.093216, -74.099080),
                           c(4.647670, 4.639937, 4.646663, 4.651681, 4.647670))
sum(inter1)
