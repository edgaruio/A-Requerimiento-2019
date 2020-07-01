# Mapa convenios
rm(list = ls())
library(dplyr); library(leaflet); library(data.table); library(rgdal); library(readxl); library(leaflet.extras)

persona <- readRDS("Data/Persona.rds") %>% 
  data.frame() %>% 
  dplyr::select(id_persona,salario,cx_persona,cy_persona,municipio_persona) %>% 
  filter(!is.na(cx_persona)) %>% 
  filter(municipio_persona == "BOGOTA D.C.") %>% 
  distinct() %>% 
  sample_n(50000)
str(persona)

convenios <- read_excel("Data/Convenios_TMS.xlsx") %>% 
  data.frame() %>% 
  filter()
str(convenios)
cate_convenio <- names(table(convenios$CATEGORIA))
length(cate_convenio)

alimentos <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("ALIMENTOS Y BEBIDAS"))
almacenes_esp <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("ALMACENES ESPECIALIZADOS"))
calzado <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("CALZADO"))
convenios_pop <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("CONVENIOS POPULARES"))
diversion_ent <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("DIVERSION Y ENTRETENIMIENTO"))
educacion <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("EDUCACION (LIBROS, IDIOMAS Y UNIVERSIDADES)"))
hogar <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("HOGAR Y DECORACION"))
salud_depo <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("SALUD, DEPORTES Y BELLEZA"))
seguros <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("SEGUROS"))
tecnologia <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("TECNOLOGIA Y ELECTRODOMESTICOS"))
tiendas <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("TIENDAS POR DEPARTAMENTO"))
turismo <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("TURISMO"))
vehiculos <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("VEHICULOS Y MOTOS"))
vestuario <- convenios %>% 
  dplyr::filter(CATEGORIA %in% c("VESTUARIO"))

# colorNumeric(palette, domain, na.color = "#808080", alpha = FALSE,
#              reverse = FALSE)

mypalette = colorBin(palette="viridis", domain=persona$salario, na.color="transparent")

# # Capas
cundi <- readRDS("Data/poligonos-localidades/Cundinamarca.rds")
localidad <- readOGR("Data/poligonos-localidades/poligonos-localidades.shp")

icon_convenio <- makeIcon(
  iconUrl = "People_Location_512.png",
  iconWidth = 20, iconHeight = 20)

map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 14))
map %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(data=localidad, fill = F, stroke = T, color = "navy", group = "study area") %>%
  addPolygons(data=cundi, fill = F, stroke = T, color = "red", group = "study area") %>%
  addMarkers(data=alimentos, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "ALIMENTOS", icon = icon_convenio) %>%
  addMarkers(data=almacenes_esp, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "ALMACENES ESP", icon = icon_convenio) %>%
  addMarkers(data=calzado, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "CALZADO", icon = icon_convenio) %>%
  addMarkers(data=convenios_pop, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "CONVENIOS POPULARES", icon = icon_convenio) %>%
  addMarkers(data=diversion_ent, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "DIVERSION Y ENTRETENIMIENTO", icon = icon_convenio) %>%
  addMarkers(data=educacion, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "EDUCACION", icon = icon_convenio) %>%
  addMarkers(data=hogar, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "HOGAR Y DECORACION", icon = icon_convenio) %>%
  addMarkers(data=salud_depo, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "SALUD Y DEPORTES", icon = icon_convenio) %>%
  addMarkers(data=seguros, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "SEGUROS", icon = icon_convenio) %>%
  addMarkers(data=tecnologia, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "TECNOLOGIA", icon = icon_convenio) %>%
  addMarkers(data=tiendas, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "TIENDAS", icon = icon_convenio) %>%
  addMarkers(data=turismo, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "TURISMO", icon = icon_convenio) %>%
  addMarkers(data=vehiculos, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "VEHICULOS", icon = icon_convenio) %>%
  addMarkers(data=vestuario, lng =~CX, lat =~CY, label=~RAZON.ZOCIAL, group = "VESTUARIO", icon = icon_convenio) %>%
  addCircleMarkers(data=persona,lng =~cx_persona, lat =~cy_persona,
                   # popup = ~as.character(Categoria),label = ~as.character(paste(Categoria)),
                   group = "Salario",
                   fillColor = ~mypalette(persona$salario), fillOpacity = 0.2,radius=2, stroke=FALSE) %>%
  addLegend(pal=mypalette, values= persona$salario, opacity=0.7, title = "Salario", position = "bottomright") %>%
  addHeatmap(data=persona,lng =~cx_persona, lat =~cy_persona, radius = 15,blur = 30, max = .3, group = "Densidad") %>%
  # addCircleMarkers(data=bd_persona(),lng =~PersonaCX, lat =~PersonaCY,
  #                  # popup = ~as.character(Categoria),label = ~as.character(paste(Categoria)),
  #                  group = "Densidad",
  #                  fillColor = ~paleta2()(bd_persona()$Estrato), fillOpacity = 0.4,radius=2, stroke=FALSE) %>%
  # addLegend(pal=paleta2(), values= bd_persona()$id, opacity=0.9, title = "Densidad", position = "bottomright",group = "Paleta2") %>%
  addLayersControl(baseGroups  = c("Salario","Densidad"),
                   overlayGroups = c("ALIMENTOS","ALMACENES ESP","CALZADO","CONVENIOS POPULARES","DIVERSION Y ENTRETENIMIENTO","EDUCACION","HOGAR Y DECORACION",
                                     "SALUD Y DEPORTES","SEGUROS","TECNOLOGIA","TIENDAS","TURISMO","VEHICULOS","VESTUARIO"),
                   options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>%
  hideGroup(c("ALIMENTOS","ALMACENES ESP","CALZADO","CONVENIOS POPULARES","DIVERSION Y ENTRETENIMIENTO","EDUCACION","HOGAR Y DECORACION",
              "SALUD Y DEPORTES","SEGUROS","TECNOLOGIA","TIENDAS","TURISMO","VEHICULOS","VESTUARIO")) %>% 
  setView(-74.078773, 4.64144452, zoom = 11)

