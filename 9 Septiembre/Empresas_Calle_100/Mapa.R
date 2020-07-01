# Librerias
library(data.table); library(dplyr); library(RODBC); library(leaflet)


#### Empresas Afilaidas
conn_geo_empresas <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/SEPTIEMBRE/Empresas_Calle100/empresas_mapinfo.accdb")
subset(sqlTables(conn_geo_empresas), tableType = "SYSTEM TABLE")
consulta_geo_emp <- sqlFetch(conn_geo_empresas, "empresasafiliadas_mes")
str(consulta_geo_emp)
odbcClose(conn_geo_empresas)

consulta_geo_emp <- consulta_geo_emp %>% 
  filter(Nom_Dep %in% c("CUNDINAMARCA", "DISTRITO CAPITAL")) %>% 
  mutate(IDEMP = as.character(IDEMP))
table(consulta_geo_emp$Nom_Dep)
str(consulta_geo_emp)

# Calee 100
m <- matrix(c(4.653382, -74.047571, 4.749071, -74.148466), ncol = 2, byrow = T)
calle100 <- data.frame(m) %>% 
  rename(CY=X1,CX=X2)
str(calle100)  

modelo <- lm(CY ~ CX, data = calle100)

emp_afil_100 <- consulta_geo_emp %>% 
  filter(CY >=  -65.5735 -0.9484 * CX)

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addCircles(data = emp_afil_100,lng =~ CX, lat =~ CY, radius = 0.01, opacity = 0.3)


####  Empresas No Afilaidas

emp_noafil <- fread("Empresas_Calle100/GeoNoAfil.csv")
str(emp_noafil)

emp_noafil_100 <- emp_noafil %>% 
  filter(CY >=  -65.5735 -0.9484 * CX)

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addCircles(data = emp_noafil_100,lng =~ CX, lat =~ CY, radius = 0.01, opacity = 0.3, color = "navy")

######################### Agregamos individual
seg_ind <- readRDS("Empresas_Calle100/ConsolidacionAGO2019.rds") %>% 
  select(id_empresa,NumEmpleados:Seg_Medio,promedio_aportes,promedio_remaneto) %>% 
  distinct()
str(seg_ind)

emp_afil_100 <- emp_afil_100 %>% 
  left_join(seg_ind, by = c("IDEMP"="id_empresa")) %>% 
  na.omit()
str(emp_afil_100)

# Exportamos
fwrite(emp_afil_100, file = "Empresas_Calle100/emp_afil_100.csv", row.names = F, sep = ";", dec = ",")
fwrite(emp_noafil_100, file = "Empresas_Calle100/emp_noafil_100.csv", row.names = F, sep = ";", dec = ",")
