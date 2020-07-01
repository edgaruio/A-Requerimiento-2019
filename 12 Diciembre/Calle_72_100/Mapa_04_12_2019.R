# Librerias
rm(list = ls())
library(dplyr); library(leaflet); library(leaflet.extras); library(sp); library(data.table); library(RODBC); library(rgeos)
library(rgdal)

personas <- readRDS("ConsolidacionFEB2020.rds") %>% 
  data.frame() %>% 
  dplyr::select(id_persona:marca_afiliado_unico,id_empresa,Piramide1,Piramide2) %>% 
  distinct() %>% 
  filter(!is.na(cx_persona))
names(personas)

empresas <- readRDS("ConsolidacionFEB2020.rds") %>%
  dplyr::select(id_empresa:Num_cesantias) %>% 
  distinct() %>% 
  filter(!is.na(cx_empresa))
str(empresas)

# Creamos poligonos
cua_72_100 <- matrix(c(4.654884, -74.055480, 
                       4.664679, -74.047069, 
                       4.671484, -74.043794, 
                       4.679917, -74.038233, 
                       4.685020, -74.047943, 
                       4.686899, -74.057238, 
                       4.678985, -74.058533,
                       4.673861, -74.067528, 
                       4.667693, -74.073345, 
                       4.654884, -74.055480),ncol = 2, byrow = T); cua_72_100 <- cbind(cua_72_100[,2],cua_72_100[,1])
cua_72_83 <- matrix(c(4.654884, -74.055480, 
                      4.664679, -74.047069, 
                      4.673861, -74.067528, 
                      4.667693, -74.073345,
                      4.654884, -74.055480),ncol = 2, byrow = T); cua_72_83 <- cbind(cua_72_83[,2],cua_72_83[,1])
cua_83_92 <- matrix(c(4.664679, -74.047069, 
                      4.671484, -74.043794,
                      4.678985, -74.058533,
                      4.673861, -74.067528,
                      4.664679, -74.047069),ncol = 2, byrow = T); cua_83_92 <- cbind(cua_83_92[,2],cua_83_92[,1])
cua_92_100 <- matrix(c(4.671484, -74.043794, 
                       4.679917, -74.038233, 
                       4.685020, -74.047943, 
                       4.686899, -74.057238, 
                       4.678985, -74.058533,
                       4.671484, -74.043794),ncol = 2, byrow = T); cua_92_100 <- cbind(cua_92_100[,2],cua_92_100[,1])


P1 <- Polygon(cua_72_100)
P2 <- Polygon(cua_72_83)
P3 <- Polygon(cua_83_92)
P4 <- Polygon(cua_92_100)

P1s <- Polygons(list(P1), 1)
P2s <- Polygons(list(P2), 1)
P3s <- Polygons(list(P3), 2)
P4s <- Polygons(list(P4), 3)

SPlgs <- SpatialPolygons(list(P2s,P3s,P4s))

df <- data.frame(ID = 1:3)

SPDF <- SpatialPolygonsDataFrame(SPlgs, df)
saveRDS(SPDF, file = "Calle72_100/Data/Capa_poligonos.rds")

plot(SPDF, axes=T, col = 'blue')
class(SPDF)


### EMPRESAS ====
inter72_83 <- point.in.polygon(empresas$cx_empresa,empresas$cy_empresa,
                           c(-74.055480,-74.047069,-74.067528,-74.073345,-74.055480),
                           c(4.654884,4.664679,4.673861,4.667693,4.654884)) %>% 
  as.matrix() %>% as.data.frame() %>% rename(calle72_83=V1); str(inter72_83)

inter83_92 <- point.in.polygon(empresas$cx_empresa,empresas$cy_empresa,
                           c(-74.047069,-74.043794,-74.058533,-74.067528,-74.047069),
                           c(4.664679,4.671484,4.678985,4.673861,4.664679)) %>% 
  as.matrix() %>% as.data.frame() %>% rename(calle83_92=V1); str(inter83_92)

inter92_100 <- point.in.polygon(empresas$cx_empresa,empresas$cy_empresa,
                           c(-74.043794,-74.038233,-74.047943,-74.057238,-74.058533,-74.043794),
                           c(4.671484,4.679917,4.685020,4.686899,4.678985,4.671484)) %>% 
  as.matrix() %>% as.data.frame() %>% rename(calle92_100=V1); str(inter92_100)
sum(inter72_83$calle72_83)

geo_empresas <- bind_cols(empresas,inter72_83,inter83_92,inter92_100) %>%
  filter(calle72_83 == 1 | calle83_92 == 1 | calle92_100 == 1) %>% 
  mutate(calle72_83 = ifelse(calle72_83 == 1, "calle72_83", NA),
         calle83_92 = ifelse(calle83_92 == 1, "calle83_92", NA),
         calle92_100 = ifelse(calle92_100 == 1, "calle92_100", NA),
         cuadrante_emp = paste(calle72_83,calle83_92,calle92_100,sep = "*")) %>% 
  mutate(cuadrante_emp = gsub("NA","",cuadrante_emp,fixed = T)) %>% 
  mutate(cuadrante_emp = gsub("*","",cuadrante_emp,fixed = T))
str(geo_empresas)
saveRDS(geo_empresas, file = "Calle72_100/Data/geo_empresas.rds")

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(provider = "CartoDB.Positron",
                   options = providerTileOptions(opacity = 1)) %>%
  addCircleMarkers(data = geo_empresas, lng =~ cx_empresa, lat =~ cy_empresa, radius = 1, opacity = 0.1) %>%
  addPolygons(data=SPDF, color = "navy")


### PERSONAS ====
inter72_83_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                                 c(-74.055480,-74.047069,-74.067528,-74.073345,-74.055480),
                                 c(4.654884,4.664679,4.673861,4.667693,4.654884)) %>% 
  as.matrix() %>% as.data.frame() %>% rename(calle72_83_p=V1); str(inter72_83_p)

inter83_92_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                                 c(-74.047069,-74.043794,-74.058533,-74.067528,-74.047069),
                                 c(4.664679,4.671484,4.678985,4.673861,4.664679)) %>% 
  as.matrix() %>% as.data.frame() %>% rename(calle83_92_p=V1); str(inter83_92_p)

inter92_100_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                                  c(-74.043794,-74.038233,-74.047943,-74.057238,-74.058533,-74.043794),
                                  c(4.671484,4.679917,4.685020,4.686899,4.678985,4.671484)) %>% 
  as.matrix() %>% as.data.frame() %>% rename(calle92_100_p=V1); str(inter92_100_p)
sum(inter72_83_p$calle72_83)

geo_personas <- bind_cols(personas,inter72_83_p,inter83_92_p,inter92_100_p) %>%
  left_join(geo_empresas %>% dplyr::select(id_empresa,calle72_83,calle83_92,calle92_100), by = "id_empresa") %>%
  filter(calle72_83 == "calle72_83" | calle83_92 == "calle83_92" | calle92_100 == "calle92_100" | calle72_83_p == 1 | calle83_92_p == 1 | calle92_100_p == 1) %>% 
  mutate(calle72_83 = ifelse(calle72_83 == "calle72_83", "calle72_83", NA),
         calle83_92 = ifelse(calle83_92 == "calle83_92", "calle83_92", NA),
         calle92_100 = ifelse(calle92_100 == "calle92_100", "calle92_100", NA),
         calle72_83_p = ifelse(calle72_83_p == 1, "calle72_83", NA),
         calle83_92_p = ifelse(calle83_92_p == 1, "calle83_92", NA),
         calle92_100_p = ifelse(calle92_100_p == 1, "calle92_100", NA),
         cuadrante_t = paste(calle72_83,calle83_92,calle92_100,sep = "*"),
         cuadrante_v = paste(calle72_83_p,calle83_92_p,calle92_100_p,sep = "*")) %>% 
  mutate(cuadrante_v = gsub("NA","",cuadrante_v,fixed = T)) %>% 
  mutate(cuadrante_v = gsub("*","",cuadrante_v,fixed = T)) %>% 
  mutate(cuadrante_t = gsub("NA","",cuadrante_t,fixed = T)) %>% 
  mutate(cuadrante_t = gsub("*","",cuadrante_t,fixed = T))
str(geo_personas)
saveRDS(geo_personas, file = "Calle72_100/Data/geo_personas.rds")

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(provider = "CartoDB.Positron",
                   options = providerTileOptions(opacity = 1)) %>%
  addCircleMarkers(data = geo_personas, lng =~ cx_persona, lat =~ cy_persona, radius = 0.2, opacity = 0.3) %>%
  addPolygons(data=SPDF, color = "navy")








#