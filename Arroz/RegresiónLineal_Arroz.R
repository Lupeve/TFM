library(RSQLite)
library(forecast)
library(ggplot2)
library(tseries)
library(DBI)
library(foreign)
library(psych)
library(MASS)
library(corrplot)
library(dplyr)
library(car)
#Ejecutar primero Descomposicion estacional Arroz (clásica, Loess y Prophet).R y seguidamente estos comandos:
#conexion a la base de datos
ruta_bd <- "C:\\Users\\a1682\\Desktop\\Luis\\Luis\\TFM desarrollo\\Base de datos\\Tablas.db"
con <- dbConnect(RSQLite::SQLite(), dbname = ruta_bd)

consulta_arroz <- "SELECT * FROM 'Tabla Arroz'"
consulta_ipc <- "SELECT * FROM 'Tabla IPC'"
consulta_paro <- "SELECT * FROM 'Paro_sexoyedad_total'"
consulta_regresores <- "SELECT * FROM 'Tabla_prophet_2'"

datos_arroz <- dbGetQuery(con, consulta_arroz)
datos_ipc <- dbGetQuery(con, consulta_ipc)
datos_paro <- dbGetQuery(con, consulta_paro)
datos_regresores <- dbGetQuery(con, consulta_regresores)

# Cerrar la conexión
dbDisconnect(con)

# Convertir la columna 'Fecha' a tipo Date
datos_arroz$Fecha <- as.Date(datos_arroz$Fecha)
datos_ipc$Fecha <- as.Date(datos_ipc$Fecha)
datos_paro$Fecha <- as.Date(datos_paro$Fecha)
datos_estacionalidad_arroz <- data.frame(forecast_arroz2)
datos_regresores$Fecha <- as.Date(datos_regresores$Fecha)

# Renombrar la columna 'ds' a 'Fecha'
names(datos_estacionalidad_arroz)[names(datos_estacionalidad_arroz) == "ds"] <- "Fecha"
datos_estacionalidad_arroz$Fecha <- as.Date(datos_estacionalidad_arroz$Fecha)

# Fusionar las tablas por 'Fecha'
datos_merged_arroz <- merge(datos_arroz, datos_ipc[, c("Fecha", "Anual(en%)")], by = "Fecha", suffixes = c("", "_ipc"))
datos_merged_arroz <- merge(datos_merged_arroz, datos_paro[, c("Fecha", "TOTAL")], by = "Fecha", suffixes = c("", "_paro"))
datos_merged_arroz <- merge(datos_merged_arroz, datos_estacionalidad_arroz[, c("Fecha", "yearly")], by = "Fecha", suffixes = c("", "_estacionalidad"))
datos_merged_arroz <- merge(datos_merged_arroz, datos_estacionalidad_arroz[, c("Fecha", "trend")], by = "Fecha", suffixes = c("", "_estacionalidad"))
datos_merged_arroz <- merge(datos_merged_arroz, datos_regresores[, c("Fecha", "lockdown1")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_arroz <- merge(datos_merged_arroz, datos_regresores[, c("Fecha", "lockdown2")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_arroz <- merge(datos_merged_arroz, datos_regresores[, c("Fecha", "lockdown3")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_arroz <- merge(datos_merged_arroz, datos_regresores[, c("Fecha", "lockdown4")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_arroz <- merge(datos_merged_arroz, datos_regresores[, c("Fecha", "lockdown5")], by = "Fecha", suffixes = c("", "_regresores"))

# Verificar la estructura del data frame fusionado
str(datos_merged_arroz)
head(datos_merged_arroz)

cor(datos_merged_arroz[, c("CONSUMOXCAPITA", "GASTOXCAPITA", "PRECIOMEDIOkg", "PENETRACION(%)", "VALOR(MilesEuros)", "VOLUMEN(MilesKg)", "Anual(en%)", "TOTAL", "yearly", "trend", "lockdown1", "lockdown2", "lockdown3", "lockdown4", "lockdown5")])

# Ajustar el modelo de regresión lineal
modelo1_arroz <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + GASTOXCAPITA + `PENETRACION(%)` + `VOLUMEN(MilesKg)` + `Anual(en%)` + TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo1_arroz)

# Ajustar el modelo de regresión lineal
modelo2_arroz <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + `Anual(en%)` + TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo2_arroz)

# Ajustar el modelo de regresión lineal
modelo3_arroz <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo3_arroz)

# Ajustar el modelo de regresión lineal
modelo4_arroz <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo4_arroz)

# Ajustar el modelo de regresión lineal
modelo5_arroz <- lm(CONSUMOXCAPITA ~ `Anual(en%)` + TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo5_arroz)

# Ajustar el modelo de regresión lineal
modelo6_arroz <- lm(CONSUMOXCAPITA ~ `Anual(en%)` + yearly + trend + lockdown1 + lockdown2, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo6_arroz)

# Ajustar el modelo de regresión lineal
modelo7_arroz <- lm(CONSUMOXCAPITA ~ yearly + trend + lockdown1 + lockdown2, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo7_arroz)

# Ajustar el modelo de regresión lineal
modelo8_arroz <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + yearly + lockdown1, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo8_arroz)

vif(modelo1_arroz)
vif(modelo2_arroz)
vif(modelo3_arroz)
vif(modelo4_arroz)
vif(modelo5_arroz)
vif(modelo6_arroz)
vif(modelo7_arroz)
vif(modelo8_arroz)

#COMPARATIVA datos originales vs Modelo2 predicho
fitted.values(modelo7_arroz)
plot(fitted.values(modelo7_arroz), type='l')

lines(datos_merged_arroz$CONSUMOXCAPITA, col='red')
summary(modelo7_arroz)
plot(fitted.values(modelo7_arroz), type='l', ylim=c(0.2, max(datos_merged_arroz$CONSUMOXCAPITA)),main="Datos originales VS valores ajustados con el modelo",
     xlab="Tiempo", ylab="CONSUMOXCAPITA (kg)")

#2.1 Análisis de los residuales de modelos lineales modelo7_arroz

#Modelado
modelo7_arroz <- lm(CONSUMOXCAPITA ~ yearly + trend + lockdown1 + lockdown2, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo7_arroz)

#Análisis de los residuales
par(mfrow = c(2, 2))
plot(modelo7_arroz$residuals,main="Dispersión de residuos")
plot(modelo7_arroz$residuals ~ modelo7_arroz$fitted.values, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
#histograma de los residuos para comprobar su normalidad
hist(modelo7_arroz$residuals, main="Histograma de residuales", xlab="Residuales")
#Gráfico NOrmal Q-Q
qqnorm(modelo7_arroz$residuals)
qqline(modelo7_arroz$residuals)

#Comprobar normalidad
jarque.bera.test(modelo7_arroz$residuals)

#2.2 Mejorar la Normalidad de los Residuos con Log
modelo7_arroz_log <- lm(log(CONSUMOXCAPITA) ~ yearly + trend + lockdown1 + lockdown2, data = datos_merged_arroz)
summary(modelo7_arroz_log)
#Análisis de los residuales
par(mfrow = c(2, 2))
plot(modelo7_arroz_log$residuals,main="Dispersión de residuos")
plot(modelo7_arroz_log$residuals ~ modelo7_arroz_log$fitted.values, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
#histograma de los residuos para comprobar su normalidad
hist(modelo7_arroz_log$residuals, main="Histograma de residuales", xlab="Residuales")
#Gráfico NOrmal Q-Q
qqnorm(modelo7_arroz_log$residuals)
qqline(modelo7_arroz_log$residuals)

#Comprobar normalidad
jarque.bera.test(modelo7_arroz_log$residuals)

#COMPARATIVA datos originales vs Modelo7_log predicho
par(mfrow = c(1, 1))
fitted_original <- exp(fitted.values(modelo7_arroz_log))
plot(fitted_original, type='l')

lines(datos_merged_arroz$CONSUMOXCAPITA, col='red')
summary(modelo7_arroz_log)
plot(fitted_original, type='l', ylim=c(0.2, max(datos_merged_arroz$CONSUMOXCAPITA)),main="Datos originales VS valores ajustados con el modelo",
     xlab="Tiempo", ylab="CONSUMOXCAPITA (kg)")

#3.1 Análisis de los residuales de modelos lineales modelo7

#Modelado
modelo8_arroz <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + yearly + lockdown1, data = datos_merged_arroz)
# Resumen del modelo
summary(modelo8_arroz)

#Análisis de los residuales
par(mfrow = c(2, 2))
plot(modelo8_arroz$residuals,main="Dispersión de residuos")
plot(modelo8_arroz$residuals ~ modelo8_arroz$fitted.values, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
#histograma de los residuos para comprobar su normalidad
hist(modelo8_arroz$residuals, main="Histograma de residuales", xlab="Residuales")
#Gráfico NOrmal Q-Q
qqnorm(modelo8_arroz$residuals)
qqline(modelo8_arroz$residuals)

#Comprobar normalidad
jarque.bera.test(modelo8_arroz$residuals)

#COMPARATIVA datos originales vs Modelo8 predicho
fitted.values(modelo8_arroz)
plot(fitted.values(modelo8_arroz), type='l')

lines(datos_merged_arroz$CONSUMOXCAPITA, col='red')
summary(modelo8_arroz)
plot(fitted.values(modelo8_arroz), type='l', ylim=c(0.2, max(datos_merged_arroz$CONSUMOXCAPITA)),main="Datos originales VS valores ajustados con el modelo",
     xlab="Tiempo", ylab="CONSUMOXCAPITA (kg)")

#4 Pronosticos del modelo a 2 años
par(mfrow = c(1, 1))
modelo7_arroz <- lm(CONSUMOXCAPITA ~ yearly + trend + lockdown1 + lockdown2, data = datos_merged_arroz)

# obtener estacionalidad y tendencia
descomp_arroz <- stl(datos_arroz_ts, s.window = "period")
plot(descomp_arroz)
estacio_arroz <- descomp_arroz$time.series[,"seasonal"]
tende_arroz <- descomp_arroz$time.series[,"trend"]
datos_estacionalidad_arroz <- data.frame(forecast_arroz2)


# proyectar la estacionalidad repitiendo la estacionalidad del último año
x_estacio_arroz <- tail(datos_merged_arroz$yearly, 12)
plot(x_estacio_arroz)
# proyectar la tendencia con un suavizado exponencial
h_tend_arroz <- forecast::holt(y=tende_arroz, h = 12, damped = FALSE)

# Crear las variables predictoras para los próximos 2 años
nuevos_datos_arroz <- data.frame(
  `Anual(en%)` = c(rep(3.4, 13), rep(2.3, 11)),  # 3.4 para dic-24 y todo 2025, 2.3 para ene-nov 2026
  yearly = rep(x_estacio_arroz, length.out = 24),  # Usar la estacionalidad proyectada
  trend = rep(h_tend_arroz$mean, length.out = 24),  # Usar la tendencia proyectada con Holt-Winters
  lockdown1  = rep(0, 24),  # Asumimos que no habrá nuevos lockdowns
  lockdown2 = rep(0, 24),
  lockdown3 = rep(0, 24),
  lockdown4 = rep(0, 24),
  lockdown5 = rep(0, 24)
)

# Renombrar la columna 'Anual' a 'Anual(en%)' para que coincida con los nombres del modelo
names(nuevos_datos_arroz)[names(nuevos_datos_arroz) == "Anual.en.."] <- "Anual(en%)"

# Asignar fechas a los nuevos datos
nuevos_datos_arroz$Fecha <- seq(from = as.Date("2023-12-01"), by = "month", length.out = 24)

# Pronóstico con el modelo de regresión múltiple a 2 años
nuevos_datos_arroz$Prediccion_CONSUMOXCAPITA <- predict(modelo7_arroz, newdata = nuevos_datos_arroz)

# Unir con los datos históricos
datos_historicos_arroz <- datos_merged_arroz
datos_historicos_arroz$Prediccion_CONSUMOXCAPITA <- fitted(modelo7_arroz)

datos_historicos_arroz$Fecha <- as.Date(datos_historicos_arroz$Fecha)

datos_completos_arroz <- merge(datos_historicos_arroz, nuevos_datos_arroz[, c("Fecha", "Prediccion_CONSUMOXCAPITA")], by = "Fecha", all.x = TRUE, all.y = TRUE)
plot(datos_completos_arroz$Fecha,datos_completos_arroz$CONSUMOXCAPITA,  type = "l", col = "black", lwd = 1, ylim=c(0.25, max(datos_historicos_arroz$CONSUMOXCAPITA)),
     main = "Pronóstico regresión multivariante a 2 años", 
     xlab = "Fecha", ylab = "Consumo per cápita (kg)")

lines(nuevos_datos_arroz$Fecha, nuevos_datos_arroz$Prediccion_CONSUMOXCAPITA, col = "red", lwd = 1, lty = 1)
legend("topright", legend = c("Histórico", "Pronóstico"), col = c("black", "red"), lwd = 1, cex = 0.95)





