rm(list = ls(all=T))
source("Protocolo/Funciones.r")

### Carga e instalacion de paquetes ----
pkgs <- c("data.table", "tidyverse", "recommenderlab")
Loadpkg(pkgs)

### Cargue de datos ----

BD <- readRDS("BD/Depuradas/data.rds")

# exportacion a python

persona <- fread("~/CamiloYate/Auxiliares/ConsolidadoPersona.csv", encoding = 'UTF-8', na.strings = c(""),
                 col.names = c('sectoreconomico','num_afiliadosingrupofamiliar','ind_aventureros','num_afiliados','num_alto','asesor','num_aventureros',
                               'ind_bancompartir','barrioempresa','barrio','bencuotamonetaria','num_bancompartir','num_basico','num_categoriac','empresacx',
                               'personacx','empresacy','personacy','num_categoriaa','num_categoriab','codlocalidad1','coordinador','descripcionciiu',
                               'descripciondivision','descripciongrupo','descripcionseccion','estadocivil','num_familiamonoparenteral',
                               'num_familiamonparenteralampliada','num_familianuclearampliada','num_familianuclearintegral','ind_famisanar','num_f',
                               'fechaafiliacion','fechanacimiento','genero','idempresa','idpersona','ind_ipscafam','ind_ipscolsubsidio',
                               'idpersonaempresasegmentoempresarial','num_joven','localidadtrabajo','localidadresidencia','latitudpobladotrabajo',
                               'latitudpobladoresidencia','longitudpobladotrabajo','longitudpobladoresidencia','num_m','num_medio','estrato','nivelacademico',
                               'noconyugue','nocuotasmonetarias','nohermanos','nohijo','nopadres','deptotrabajo','deptoresidencia','ciudadtrabajo',
                               'ciudadresidencia','pobladotrabajo','pobladoresidencia','nombreempresa','nombreapotante','numberofrecords','numeroidpersona',
                               'numerosucursales','ind_otrasips','num_pacsuramericana','num_parejaconyugal','ind_pensobligatorias','ind_pensvoluntarias',
                               'piramide1','piramide2','ind_proteccion','ind_suramericana','salario','segmentogf','segmentopoblacional','num_suramericana',
                               'ind_tienepacfamisanar','ind_tienepacsuramericana','ind_tuclub','tipoaportante','tipopobladotrabajo','tipopobladoresidencia',
                               'totalnogf','num_tuclub','categoria','idempresa2','totalnumerogrupofamiliar')
) %>% 
  distinct() %>% 
  mutate_at(c('empresacx','personacx','empresacy','personacy', 'latitudpobladotrabajo','latitudpobladoresidencia','longitudpobladotrabajo',
              'longitudpobladoresidencia'), funs(as.numeric(gsub(",",".",.)))) %>% 
  mutate(fechanacimiento=as.Date(fechanacimiento, "%d/%m/%Y"),
         NumIdPersona=as.numeric(gsub("\\D","",idpersona))) %>% 
  select(idpersona, NumIdPersona,categoria, segmentopoblacional, sectoreconomico, ind_aventureros, num_afiliados, bencuotamonetaria, 
         num_basico,num_medio,num_joven, num_alto, num_categoriaa, num_categoriab, num_categoriac,
         empresacx, empresacy, personacx, personacy, descripciondivision, descripcionseccion,
         estadocivil, fechanacimiento, genero, estrato, nivelacademico,segmentogf, noconyugue, nocuotasmonetarias, nohermanos, nohijo,
         nopadres, totalnogf, ciudadresidencia, piramide1, piramide2, salario)

BD_ <- BD %>% 
  filter(!(Categoria %in%  c("RETAIL", "DISPOSITIVOS MEDICOS"))) %>% 
  select(prod_id, Ciudad, PatologiaProd=Patologia, CategoriaProd=Categoria, NumIdcliente) %>% 
  left_join(persona, by=c("NumIdcliente"="NumIdPersona")) %>% 
  mutate(Edad=Calcular.Edad(fechanacimiento, as.Date("2018-06-30"))) %>% 
  select(NumIdcliente, prod_id, PatologiaProd,Edad, CategoriaProd, categoria:sectoreconomico, estadocivil, genero, estrato, segmentogf,
         ciudadresidencia, salario) %>% 
  mutate(Edad=Hmisc::cut2(Edad, g = 5),
         salario=Hmisc::cut2(salario, g = 5)) %>% 
  arrange(NumIdcliente, prod_id) %>%
  group_by(NumIdcliente, prod_id) %>%
  filter(row_number()==1)

fwrite(BD_, "BD/Depuradas/Base.csv")

max(BD$Fecha)

### Listado Productos m?s frecuentes.

n_top <- 1000

prods <- BD %>% 
  filter(!(Categoria %in%  c("RETAIL", "DISPOSITIVOS MEDICOS"))) %>% 
  group_by(prod_nombre) %>% 
  summarise(freq=n()) %>% 
  top_n(n_top, freq)
  
### Contruccion matriz binaria de usuarios por producto ----

aux1 <- BD  %>% 
  inner_join(prods) %>% 
  select(NumIdcliente, prod_nombre, principioconcatenado) %>% 
  unique() %>% 
  mutate(Ind=1) %>% 
  spread(key=prod_nombre, value = Ind, fill=0)

rm(BD, prods)

ids <- aux1 %>% select(NumIdcliente) %>% distinct()
saveRDS(ids, "ids.rds")

matriz_prod = as(as.matrix(aux1[,-1]) , 'binaryRatingMatrix')
rownames(matriz_prod) <- aux1[,1]
rm(BD, prods, aux1)

saveRDS(matriz_prod, "BD/Depuradas/matriz.rds")
matriz_prod <- readRDS("BD/Depuradas/matriz.rds")

matriz_prod

### Sistemas de Recomendacion Calibraci?n----
set.seed(31415)
e <- evaluationScheme(matriz_prod[1:10000],  method="cross-validation", k=3, given=-1)
n_recommendations <- c(1, 5, seq(10, 100, 10))

## UBCF User-based Collaborative Filtering

recommenderRegistry$get_entry("UBCF", dataType = "binaryRatingMatrix")

# Definicion de Modelos a Evaluar.
vec_nn <- c(5,20,50,100)
modelos_UBCF <- lapply(vec_nn, function(n){
  list(name = "UBCF", param = list(nn =n))
})
names(modelos_UBCF) <- paste0("UBCF_nn_", vec_nn)

# Evaluaci?n de Modelos
res_UBCF <- evaluate(e, method = modelos_UBCF, n= n_recommendations)

# Comparaci?n de Modelos
plot(res_UBCF, legend="topleft", annotate = F)
title("Curva ROC")
plot(res_UBCF, legend="topright", "prec/rec", annotate = F)
title("Precision / Recall")

# Metricas mejor Modelo
avg_matrices <- lapply(res_UBCF, avg)
avg_matrices$UBCF_nn_100[,5:8]

## IBCF Item-based Collaborative Filtering

recommenderRegistry$get_entry("IBCF", dataType = "binaryRatingMatrix")

# Definicion de Modelos a Evaluar.
vec_k <- c(1,5,10,20,50)
modelos_IBCF <- lapply(vec_k, function(k){
  list(name = "IBCF", param = list(k =k))
})
names(modelos_IBCF) <- paste0("IBCF_k_", vec_k)

# Evaluaci?n de Modelos
res_IBCF <- evaluate(e, method = modelos_IBCF, n= n_recommendations)

# Comparaci?n de Modelos
plot(res_IBCF, legend="topleft", annotate = F)
title("Curva ROC")
plot(res_IBCF, legend="topright", "prec/rec", annotate = F)
title("Precisi?n / Recall")

# Metricas mejor Modelo
avg_matrices <- lapply(res_IBCF, avg)
avg_matrices$IBCF_k_5[,5:8]

## ALS Alternating Least Squares
recommenderRegistry$get_entry("ALS", dataType = "binaryRatingMatrix")

# Definicion de Modelos a Evaluar.
vec_nf <- c(1,5,10,20,50)
vec_l <- c(0.1,0.3,0.5,0.7,1)
modelos_ALS <- lapply(vec_l, function(n){
  list(name = "ALS", param = list(n_factors =20, lambda=vec_l))
})
names(modelos_ALS) <- paste0("ALS", vec_l)

# Evaluaci?n de Modelos
res_ALS <- evaluate(e, method = modelos_ALS, n= n_recommendations)

# Comparaci?n de Modelos
plot(res_ALS, legend="topleft", annotate = F)
title("Curva ROC")
plot(res_ALS, legend="topright", "prec/rec", annotate = F)
title("Precisi?n / Recall")

# Metricas mejor Modelo
avg_matrices_ALS <- lapply(res_ALS, avg)
avg_matrices_ALS$ALS50[,5:8]

### Modelo Competencia ---- 

set.seed(31415)
e <- evaluationScheme(matriz_prod[1:10000],  method="cross-validation", k=10, given=-1)
n_recommendations <- c(1, 5, seq(10, 100, 10))

algorithms <- list(
  RANDOM = list(name = "RANDOM"),
  POPULAR = list(name = "POPULAR"),
  UBCF = list(name = "UBCF", param = list(nn =50)),
  IBCF = list(name = "IBCF", param = list(k =5)),
  ALS = list(name = "ALS", param = list(n_factors =20, lambda=0.3))
)
evlist <- evaluate(e, algorithms, n=seq(1, 100, 5))
plot(evlist, legend="topleft", annotate = F)
title("Curva ROC")
plot(evlist, legend="topright", "prec/rec", annotate = TRUE, main = "Precision-recall")
title("Precisi?n / Recall")

class(evlist[[5]])

vec_nf <- c(20)
vec_l <- c(0.3)
modelos_ALS <- lapply(vec_l, function(n){
  list(name = "ALS", param = list(n_factors =20, lambda=vec_l))
})
names(modelos_ALS) <- paste0("ALS", vec_l)

# Evaluaci?n de Modelos
ALS <- evaluate(e, method = modelos_ALS, n= n_recommendations)


### Preparaci?n Resultados

matriz_prod <- readRDS("BD/Depuradas/matriz.rds")

recommender <- Recommender(matriz_prod, method="UBCF", parameter=list(nn =100))
recommender <- Recommender(matriz_prod, method="RANDOM")
#recommender <- Recommender(matriz_prod, method="ALS", parameter=list(n_factors =20, lambda=0.3))

saveRDS(recommender, "Resultados/recommender.rds")
recommender <- readRDS("Resultados/recommender.rds")

ids <- readRDS("ids.rds")

res=data.frame()
for(i in 1:dim(ids)[1]){
  predictions <- predict(recommender, matriz_prod[i], n=10)
  pred_list<- getList(predictions, decode=T)
  Predicciones <- as.data.frame(do.call(rbind, pred_list))
  res=rbind(res, Predicciones)
}

res <- cbind(ids[1:dim(res)[1],], res)
names(res) <- c("NumIdcliente",paste0("Opcion",1:10))

res %>% filter(NumIdcliente==1014199488)

saveRDS(res, "APP/Data/recomendaciones.rds")
#res <- readRDS("APP/Data/recomendaciones.rds") %>% select(-NumIdcliente)

matriz_prod[1:1]

