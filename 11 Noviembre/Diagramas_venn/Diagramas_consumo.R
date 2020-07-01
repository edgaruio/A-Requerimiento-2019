# Cargamos librerias
rm(list = ls())
options(scipen = 999)
library(RODBC); library(dplyr); library(data.table); library(esquisse); library(tictoc)
library(VennDiagram)
library(RColorBrewer)

consolidada <- readRDS("Digramas_ven22112019/ConsolidacionOCT2019.rds") %>% 
  select(id_persona,NumIdPersona,Categoria,segmento_grupo_familiar) %>% 
  distinct() %>% 
  mutate(NumIdPersona = as.character(NumIdPersona),
         Categoria = ifelse(is.na(as.character(Categoria)),"Sin informacion", as.character(Categoria)),
         segmento_grupo_familiar = ifelse(is.na(segmento_grupo_familiar),"Sin informacion", segmento_grupo_familiar)) %>% 
  data.frame()
str(consolidada)

consumos <- fread("Digramas_ven22112019/consumo.csv", encoding = 'UTF-8') %>% 
  mutate(CS = rowSums(.[,c("CS_As_Hipo","CS_As_Libr","CS_As_NoLibranza","CS_As_Titular","CS_CO_CsConv")], na.rm = T),
         RyT = rowSums(.[,c(9:20)], na.rm = T),
         Salud = rowSums(.[,c("Salud_NoPos","Salud_Pos")],na.rm = T)) %>% 
  select(NumDoc,CS,MS_Drog,MS_Sup,RyT,Salud) %>% 
  mutate(NumDoc = as.character(NumDoc)) %>% 
  distinct() %>% 
  group_by(NumDoc) %>% 
  summarise_all(.funs = max)
str(consumos)
table(duplicated(consumos$NumDoc))

df_union <- consumos %>% 
  inner_join(consolidada, by =c("NumDoc"="NumIdPersona")) %>% 
  na.omit()
str(df_union)
table(df_union$segmento_grupo_familiar)

venn_sup <- df_union %>% filter(MS_Sup == 1) %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_sup_A <- df_union %>% filter(MS_Sup == 1 & Categoria == "A") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_sup_B <- df_union %>% filter(MS_Sup == 1 & Categoria == "B") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_sup_C <- df_union %>% filter(MS_Sup == 1 & Categoria == "C") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_sup_mono <- df_union %>% filter(MS_Sup == 1 & segmento_grupo_familiar == "FAMILIA MONOPARENTERAL") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_sup_mono_ampliada <- df_union %>% filter(MS_Sup == 1 & segmento_grupo_familiar == "FAMILIA MONPARENTERAL AMPLIADA") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_sup_nuclear_ampliada <- df_union %>% filter(MS_Sup == 1 & segmento_grupo_familiar == "FAMILIA NUCLEAR AMPLIADA") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_sup_nuclear_intergral <- df_union %>% filter(MS_Sup == 1 & segmento_grupo_familiar == "FAMILIA NUCLEAR INTEGRAL") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_sup_pareja <- df_union %>% filter(MS_Sup == 1 & segmento_grupo_familiar == "PAREJA CONYUGAL") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_sup_sin_info <- df_union %>% filter(MS_Sup == 1 & segmento_grupo_familiar == "Sin informacion") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()

venn_ryt <- df_union %>% filter(RyT == 1) %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_ryt_A <- df_union %>% filter(RyT == 1 & Categoria == "A") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_ryt_B <- df_union %>% filter(RyT == 1 & Categoria == "B") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_ryt_C <- df_union %>% filter(RyT == 1 & Categoria == "C") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_ryt_mono <- df_union %>% filter(RyT == 1 & segmento_grupo_familiar == "FAMILIA MONOPARENTERAL") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_ryt_mono_ampliada <- df_union %>% filter(RyT == 1 & segmento_grupo_familiar == "FAMILIA MONPARENTERAL AMPLIADA") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_ryt_nuclear_ampliada <- df_union %>% filter(RyT == 1 & segmento_grupo_familiar == "FAMILIA NUCLEAR AMPLIADA") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_ryt_nuclear_intergral <- df_union %>% filter(RyT == 1 & segmento_grupo_familiar == "FAMILIA NUCLEAR INTEGRAL") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_ryt_pareja <- df_union %>% filter(RyT == 1 & segmento_grupo_familiar == "PAREJA CONYUGAL") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_ryt_sin_info <- df_union %>% filter(RyT == 1 & segmento_grupo_familiar == "Sin informacion") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()


venn_salud <- df_union %>% filter(Salud == 1) %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_salud_A <- df_union %>% filter(Salud == 1 & Categoria == "A") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_salud_B <- df_union %>% filter(Salud == 1 & Categoria == "B") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_salud_C <- df_union %>% filter(Salud == 1 & Categoria == "C") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_Salud_mono <- df_union %>% filter(Salud == 1 & segmento_grupo_familiar == "FAMILIA MONOPARENTERAL") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_salud_mono_ampliada <- df_union %>% filter(Salud == 1 & segmento_grupo_familiar == "FAMILIA MONPARENTERAL AMPLIADA") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_salud_nuclear_ampliada <- df_union %>% filter(Salud == 1 & segmento_grupo_familiar == "FAMILIA NUCLEAR AMPLIADA") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_salud_nuclear_intergral <- df_union %>% filter(Salud == 1 & segmento_grupo_familiar == "FAMILIA NUCLEAR INTEGRAL") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_salud_pareja <- df_union %>% filter(Salud == 1 & segmento_grupo_familiar == "PAREJA CONYUGAL") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()
venn_salud_sin_info <- df_union %>% filter(Salud == 1 & segmento_grupo_familiar == "Sin informacion") %>%  select(id_persona) %>% na.omit() %>% as.matrix() %>% as.vector()


# Load library
myCol <- brewer.pal(3, "Pastel2")
ta
#### Global ====
venn.diagram(
  x = list(venn_salud, venn_ryt, venn_sup),
  main = "Participación Consumos (Global)",
  main.cex = 0.5,
  main.col = "gray",
  sub = "Afiliados",
  sub.cex = 0.2,
  sub.col = "gray",
  category.names = c("Salud" , "RyT" , "Supermercado"),
  filename = 'Digramas_ven22112019/Venn_global.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 600 , 
  width = 600 , 
  resolution = 400,
  compression = "lzw",
  
  # Circles
  lwd = 1,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .3,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.3,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1,
  print.mode=c("raw", "percent"),
  cat.col = "gray"
)


#### Categoria ====
venn.diagram(
  x = list(venn_salud_C, venn_ryt_C, venn_sup_C),
  main = "Participación Consumos (Categoria C)",
  main.cex = 0.5,
  main.col = "gray",
  sub = "Afiliados",
  sub.cex = 0.2,
  sub.col = "gray",
  category.names = c("Salud" , "RyT" , "Supermercado"),
  filename = 'Digramas_ven22112019/Venn_global_C.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 600 , 
  width = 600 , 
  resolution = 400,
  compression = "lzw",
  
  # Circles
  lwd = 1,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .3,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.3,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1,
  print.mode=c("raw", "percent"),
  cat.col = "gray"
)


#### Segmento Familiar ====
venn.diagram(
  x = list(venn_salud_pareja, venn_ryt_pareja, venn_sup_pareja),
  main = "Consumos (Pareja Conyugal)",
  main.cex = 0.5,
  main.col = "gray",
  sub = "Afiliados",
  sub.cex = 0.2,
  sub.col = "gray",
  category.names = c("Salud" , "RyT" , "Supermercado"),
  filename = 'Digramas_ven22112019/Venn_pareja_conyugal.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 600 , 
  width = 600 , 
  resolution = 400,
  compression = "lzw",
  
  # Circles
  lwd = 1,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .3,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.3,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1,
  print.mode=c("raw", "percent"),
  cat.col = "gray"
)


