# Librerias
rm(list = ls())
library(dplyr); library(leaflet); library(leaflet.extras); library(sp); library(data.table); library(RODBC); library(rgeos)
library(rgdal)

personas <- readRDS("ConsolidacionFEB2020.rds") %>% 
  data.frame() %>% 
  dplyr::select(id_persona:marca_afiliado_unico,id_empresa,Piramide1,Piramide2) %>% 
  filter(marca_afiliado_unico) %>% 
  filter(!is.na(cx_persona))
names(personas)

empresas <- readRDS("ConsolidacionFEB2020.rds") %>%
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
saveRDS(SPDF, file = "Calle_26/Data/Capa_poligonos.rds")

plot(SPDF, axes=T, col = 'blue')
class(SPDF)


### EMPRESAS ====
inter1 <- point.in.polygon(empresas$cx_empresa,
                           empresas$cy_empresa,
                           c(-74.099080, -74.094317, -74.086305, -74.093216, -74.099080),
                           c(4.647670, 4.639937, 4.646663, 4.651681, 4.647670)) %>% as.matrix() %>% as.data.frame() %>% rename(can=V1) %>% 
  mutate(can = ifelse(can == 1, "can", NA)); str(inter1)
inter2 <- point.in.polygon(empresas$cx_empresa,
                           empresas$cy_empresa,
                           c(-74.103781, -74.110565, -74.100811, -74.094929, -74.103781),
                           c(4.653254, 4.645125, 4.635513, 4.639614, 4.653254)) %>% as.matrix() %>% as.data.frame() %>% rename(gran_estacion=V1) %>% 
  mutate(gran_estacion = ifelse(gran_estacion == 1, "gran_estacion", NA)); str(inter2)
inter3 <- point.in.polygon(empresas$cx_empresa,
                           empresas$cy_empresa,
                           c(-74.103265, -74.099512, -74.106946, -74.109081, -74.103265),
                           c(4.654073, 4.658641, 4.665079, 4.663085, 4.654073)) %>% as.matrix() %>% as.data.frame() %>% rename(el_tiempo=V1) %>% 
  mutate(el_tiempo = ifelse(el_tiempo == 1, "el_tiempo", NA)); str(inter3)
inter4 <- point.in.polygon(empresas$cx_empresa,
                           empresas$cy_empresa,
                           c(-74.111902, -74.120706, -74.110524, -74.103686, -74.111902),
                           c(4.666168, 4.656071, 4.645033, 4.653299, 4.666168)) %>% as.matrix() %>% as.data.frame() %>% rename(salitre=V1) %>% 
  mutate(salitre = ifelse(salitre == 1, "salitre", NA)); str(inter4)
inter5 <- point.in.polygon(empresas$cx_empresa,
                           empresas$cy_empresa,
                           c(-74.119317, -74.111336, -74.107011, -74.116054, -74.119317),
                           c(4.679352, 4.666739, 4.671367, 4.682452, 4.679352)) %>% as.matrix() %>% as.data.frame() %>% rename(dian=V1) %>% 
  mutate(dian = ifelse(dian == 1, "dian", NA)); str(inter5)
inter6 <- point.in.polygon(empresas$cx_empresa,
                           empresas$cy_empresa,
                           c(-74.119954, -74.125295, -74.115948, -74.111882, -74.119954),
                           c(4.678686, 4.672063, 4.661628, 4.666256, 4.678686)) %>% as.matrix() %>% as.data.frame() %>% rename(dorado=V1) %>% 
  mutate(dorado = ifelse(dorado == 1, "dorado", NA)); str(inter6)
inter7 <- point.in.polygon(empresas$cx_empresa,
                           empresas$cy_empresa,
                           c(-74.124709, -74.119295, -74.116749, -74.120611, -74.124709),
                           c(4.685237, 4.679248, 4.681655, 4.687045, 4.685237)) %>% as.matrix() %>% as.data.frame() %>% rename(conecta=V1) %>% 
  mutate(conecta = ifelse(conecta == 1, "conecta", NA)); str(inter7)
inter8 <- point.in.polygon(empresas$cx_empresa,
                           empresas$cy_empresa,
                           c(-74.124865, -74.130423, -74.125358, -74.119908, -74.124865),
                           c(4.684473, 4.677971, 4.672197, 4.678698, 4.684473)) %>% as.matrix() %>% as.data.frame() %>% rename(patios_trans=V1) %>% 
  mutate(patios_trans = ifelse(patios_trans == 1, "patios_trans", NA)); str(inter8)

geo_empresas <- bind_cols(empresas,inter1,inter2,inter3,inter4,inter5,inter6,inter7,inter8) %>% 
  filter(can == "can" | gran_estacion == "gran_estacion" | el_tiempo == "el_tiempo" | salitre == "salitre" | dian == "dian" | dorado == "dorado" | conecta == "conecta" | patios_trans == "patios_trans") %>% 
  mutate(cuadrante_emp = paste(can,gran_estacion,el_tiempo,salitre,dian,dorado,conecta,patios_trans,sep = "*")) %>% 
  mutate(cuadrante_emp = gsub("NA","",cuadrante_emp,fixed = T)) %>% 
  mutate(cuadrante_emp = gsub("*","",cuadrante_emp,fixed = T))
str(geo_empresas)
saveRDS(geo_empresas, file = "Calle_26/Data/geo_empresas.rds")

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(provider = "CartoDB.Positron",
                   options = providerTileOptions(opacity = 1)) %>%
  addCircleMarkers(data = geo_empresas, lng =~ cx_empresa, lat =~ cy_empresa, radius = 1, opacity = 0.1) %>%
  addPolygons(data=SPDF, color = "navy") 


### PERSONAS ====
inter1_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                             c(-74.099080, -74.094317, -74.086305, -74.093216, -74.099080),
                             c(4.647670, 4.639937, 4.646663, 4.651681, 4.647670)) %>% as.matrix() %>% as.data.frame() %>% rename(can=V1) %>% 
  mutate(can = ifelse(can == 1, "can", NA)); str(inter1_p)
inter2_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                             c(-74.103781, -74.110565, -74.100811, -74.094929, -74.103781),
                             c(4.653254, 4.645125, 4.635513, 4.639614, 4.653254)) %>% as.matrix() %>% as.data.frame() %>% rename(gran_estacion=V1) %>% 
  mutate(gran_estacion = ifelse(gran_estacion == 1, "gran_estacion", NA)); str(inter2_p)
inter3_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                             c(-74.103265, -74.099512, -74.106946, -74.109081, -74.103265),
                             c(4.654073, 4.658641, 4.665079, 4.663085, 4.654073)) %>% as.matrix() %>% as.data.frame() %>% rename(el_tiempo=V1) %>% 
  mutate(el_tiempo = ifelse(el_tiempo == 1, "el_tiempo", NA)); str(inter3_p)
inter4_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                             c(-74.111902, -74.120706, -74.110524, -74.103686, -74.111902),
                             c(4.666168, 4.656071, 4.645033, 4.653299, 4.666168)) %>% as.matrix() %>% as.data.frame() %>% rename(salitre=V1) %>% 
  mutate(salitre = ifelse(salitre == 1, "salitre", NA)); str(inter4_p)
inter5_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                             c(-74.119317, -74.111336, -74.107011, -74.116054, -74.119317),
                             c(4.679352, 4.666739, 4.671367, 4.682452, 4.679352)) %>% as.matrix() %>% as.data.frame() %>% rename(dian=V1) %>% 
  mutate(dian = ifelse(dian == 1, "dian", NA)); str(inter5_p)
inter6_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                             c(-74.119954, -74.125295, -74.115948, -74.111882, -74.119954),
                             c(4.678686, 4.672063, 4.661628, 4.666256, 4.678686)) %>% as.matrix() %>% as.data.frame() %>% rename(dorado=V1) %>% 
  mutate(dorado = ifelse(dorado == 1, "dorado", NA)); str(inter6_p)
inter7_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                             c(-74.124709, -74.119295, -74.116749, -74.120611, -74.124709),
                             c(4.685237, 4.679248, 4.681655, 4.687045, 4.685237)) %>% as.matrix() %>% as.data.frame() %>% rename(conecta=V1) %>% 
  mutate(conecta = ifelse(conecta == 1, "conecta", NA)); str(inter7_p)
inter8_p <- point.in.polygon(personas$cx_persona,personas$cy_persona,
                             c(-74.124865, -74.130423, -74.125358, -74.119908, -74.124865),
                             c(4.684473, 4.677971, 4.672197, 4.678698, 4.684473)) %>% as.matrix() %>% as.data.frame() %>% rename(patios_trans=V1) %>% 
  mutate(patios_trans = ifelse(patios_trans == 1, "patios_trans", NA)); str(inter8_p)

geo_personas <- bind_cols(personas,inter1_p,inter2_p,inter3_p,inter4_p,inter5_p,inter6_p,inter7_p,inter8_p) %>% 
  left_join(geo_empresas %>% dplyr::select(id_empresa,cuadrante_emp), by = "id_empresa") %>% 
  mutate(cuadrante_v = paste(can,gran_estacion,el_tiempo,salitre,dian,dorado,conecta,patios_trans,sep = "*")) %>% 
  mutate(cuadrante_v = gsub("NA","",cuadrante_v,fixed = T)) %>% 
  mutate(cuadrante_v = gsub("*","",cuadrante_v,fixed = T)) %>% 
  filter(cuadrante_v != "" | !is.na(cuadrante_emp))
str(geo_personas)
table(geo_personas$cuadrante_v)
saveRDS(geo_personas, file = "Calle_26/Data/geo_personas.rds")

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(provider = "CartoDB.Positron",
                   options = providerTileOptions(opacity = 1)) %>%
  addCircleMarkers(data = geo_personas, lng =~ cx_persona, lat =~ cy_persona, radius = 0.2, opacity = 0.3) %>%
  addPolygons(data=SPDF, color = "navy") 
