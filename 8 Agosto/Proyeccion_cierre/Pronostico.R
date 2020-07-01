# Conectamos a access - Pronosticos para: 
# Cuota monetaria girada, kit escolar, Bono lonchera & otros subsidios
rm(list = ls())
options(scipen = 999)
library(RODBC); library(data.table); library(dplyr); library(tidyr); library(forecast); library(ggplot2); library(readxl)

conn_cuota_monetaria <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/AGOSTO/Proyeccion Cierre 2019/GiroCuotaMonetaria2018.accdb")
subset(sqlTables(conn_cuota_monetaria), tableType = "SYSTEM TABLE")
consulta_cm <- sqlFetch(conn_cuota_monetaria, "giro_cuotamonetaria")
str(consulta_cm)
odbcClose(conn_cuota_monetaria)

consulta_cm <- consulta_cm %>% 
  mutate(id_persona = as.character(id_persona),
         anio_mes = paste(año,mes,sep="_")) 

bd_promo_2017 <- read_excel("CM.xlsx") %>% 
  data.frame() %>% 
  select(AÑO,MES,Proxi_cm,SUBSIDIO) %>% 
  mutate(fecha = paste(AÑO,MES,01,sep="/")) %>% 
  mutate(fecha = as.Date.character(fecha, format = "%Y/%m/%d")) %>% 
  select(fecha,Proxi_cm,SUBSIDIO) %>% 
  dplyr::rename(n_cuota_girada=Proxi_cm,
                dinero_cuota=SUBSIDIO) %>% 
  arrange(fecha)
str(bn_promo_2017)

bd_promo_2018 <- consulta_cm %>% 
  mutate(n_cuota_girada = round(ifelse(año == 2018, valor/31400, valor/33400),0),
         fecha = paste(año,mes,01,sep = "/"),
         fecha = as.Date.character(fecha, format = "%Y/%m/%d")) %>% 
  group_by(fecha) %>% 
  summarise(n_cuota_girada = sum(n_cuota_girada, na.rm = T),
            dinero_cuota = sum(valor, na.rm = T)) %>% 
  arrange(fecha)
str(bd_promo_2018)

# test <- consulta_cm %>% 
#   group_by(id_persona,anio_mes) %>% 
#   summarise(cuenta = n()) %>% 
#   arrange(desc(cuenta))
# str(test)

bd_prono <- rbind(bd_promo_2017,bd_promo_2018)
str(bd_prono)

# Pronosotico
ts_cm <- ts(bd_prono$n_cuota_girada, start = c(2013,1), frequency = 12)
summary(ts_cm)
plot(ts_cm)
tsdisplay(ts_cm)
acf(ts_cm)
pacf(ts_cm)

# Ajuste de modelo
#Hollwinters
foreach <- HoltWinters(ts_cm)
summary(foreach)
forecast(foreach, h=4, level = c(.5))
jpeg(filename = "./RESULTADOS/Pro_CM.jpeg", width = 700, height = 480, res = 100)
autoplot(forecast(foreach, h=16, level = c(0.3)), main = "Pronóstico - Cuota Monetaria", 
         xlab="Año", ylab = "Cuotas", lwd = 2) + theme_bw() + theme(legend.position="none")
dev.off()
Tb_TSSUB <- data.frame(forecast(foreach, h=4, level = c(0.30,.20)))
write.csv(Tb_TSSUB, file = "/RESULTADOS/Tb_CM.csv")


fit_bono <- Arima(ts_cm, order=c(1,1,1), seasonal=c(0,0,0))
fit_bono
t(confint(fit1))
jpeg(filename = "./RESULTADOS/Pro_CM.jpeg", width = 700, height = 480, res = 100)
autoplot(forecast(fit1, h=12, level = c(.20)), main = "Pronóstico - Subsidio", 
         xlab="Año", ylab = "Cuotas Monetarias", lwd = 2) + theme_bw() + theme(legend.position="none")
dev.off()
Tb_TSSUB <- data.frame(forecast(fit1, h=57, level = c(.20)))
write.csv(Tb_TSSUB, file = "./RESULTADOS/Tablas/Tb_TSSUB.csv")


# Pronóstico Bono Lonchera

conn_bono_lonchera <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/AGOSTO/Proyeccion Cierre 2019/BonoLonchera.accdb")
subset(sqlTables(conn_bono_lonchera), tableType = "SYSTEM TABLE")
consulta_bono <- sqlFetch(conn_bono_lonchera, "BonoLochera")
str(consulta_bono)
odbcClose(conn_bono_lonchera)

df_bono <- consulta_bono %>% 
  mutate(fecha = as.Date.character(paste(AÑO,MES,01,sep="/"),format = "%Y/%m/%d")) %>% 
  group_by(fecha) %>% 
  summarise(conteo = n())
str(df_bono)

ts_bono <- ts(df_bono$conteo, start = c(2017,9), frequency = 12)
summary(ts_bono)
plot(ts_bono)
tsdisplay(ts_bono)
acf(ts_bono)
pacf(ts_bono)

# Pronostico
# fore_bono <- HoltWinters(ts_bono, beta = 0, gamma = 0, seasonal = c("additive"))
# summary(fore_bono)
# forecast(foreach, h=4, level = c(.5))
# jpeg(filename = "./RESULTADOS/Pro_CM.jpeg", width = 700, height = 480, res = 100)
# autoplot(forecast(foreach, h=16, level = c(0.3)), main = "Pronóstico - Cuota Monetaria", 
#          xlab="Año", ylab = "Cuotas", lwd = 2) + theme_bw() + theme(legend.position="none")
# dev.off()
# Tb_TSSUB <- data.frame(forecast(foreach, h=4, level = c(0.30,.20)))
# write.csv(Tb_TSSUB, file = "/RESULTADOS/Tb_CM.csv")

fit_bono <- Arima(ts_bono, order=c(1,1,1), seasonal=c(0,0,0))
fit_bono
t(confint(fit_bono))

# Kit escolar 2019
conn_kit2019 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/AGOSTO/Proyeccion Cierre 2019/Kit_Escolar_2019.accdb")
subset(sqlTables(conn_kit2019), tableType = "SYSTEM TABLE")
kit2019 <- sqlFetch(conn_kit2019, "base_kit_escolar_2019")
str(kit2019)
odbcClose(conn_kit2019)

df_kit2019 <- kit2019 %>% 
  summarise(n_2019 = n())
str(df_kit2019)  

# Kit escolar 2018
conn_kit2018 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/AGOSTO/Proyeccion Cierre 2019/Kit_Escolar_2018.accdb")
subset(sqlTables(conn_kit2018), tableType = "SYSTEM TABLE")
kit2018 <- sqlFetch(conn_kit2018, "Kit_escolar_consolidado")
str(kit2018)
odbcClose(conn_kit2018)

df_kit2018 <- kit2018 %>% 
  summarise(n_2018 = sum(Total_kit_escolar_aprobado, na.rm = T))
str(df_kit2018)  

# Kit escolar 2017
conn_kit2017 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/A REQUERIMIENTO/AGOSTO/Proyeccion Cierre 2019/Kit_Escolar_2017.accdb")
subset(sqlTables(conn_kit2017), tableType = "SYSTEM TABLE")
kit2017 <- sqlFetch(conn_kit2017, "Kitescolar")
str(kit2017)
odbcClose(conn_kit2017)

df_kit2017 <- kit2017 %>% 
  summarise(n_2017 = n())
str(df_kit2017)  

kit <- data.frame(cbind(df_kit2019, df_kit2018, df_kit2017))
kit
# Exportar datos

fwrite(df_bono, "df_bono.csv", row.names = F)
fwrite(bd_prono, "df_cm.csv", row.names = F)
fwrite(kit, "kit.csv", row.names = F)
