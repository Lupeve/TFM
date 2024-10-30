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
#Ejecutar primero Descomposicion estacional total carne (clásica, Loess y Prophet).R y seguidamente estos comandos:
#conexion a la base de datos
ruta_bd <- "C:\\Users\\a1682\\Desktop\\Luis\\Luis\\TFM desarrollo\\Base de datos\\Tablas.db"
con <- dbConnect(RSQLite::SQLite(), dbname = ruta_bd)

consulta_huevos <- "SELECT * FROM 'Tabla Huevos unds'"
consulta_ipc <- "SELECT * FROM 'Tabla IPC'"
consulta_paro <- "SELECT * FROM 'Paro_sexoyedad_total'"
consulta_regresores <- "SELECT * FROM 'Tabla_prophet_2'"

datos_huevos <- dbGetQuery(con, consulta_huevos)
datos_ipc <- dbGetQuery(con, consulta_ipc)
datos_paro <- dbGetQuery(con, consulta_paro)
datos_regresores <- dbGetQuery(con, consulta_regresores)

# Cerrar la conexión
dbDisconnect(con)

# Convertir la columna 'Fecha' a tipo Date
datos_huevos$Fecha <- as.Date(datos_huevos$Fecha)
datos_ipc$Fecha <- as.Date(datos_ipc$Fecha)
datos_paro$Fecha <- as.Date(datos_paro$Fecha)
datos_estacionalidad_huevos <- data.frame(forecast_huevos2)
datos_regresores$Fecha <- as.Date(datos_regresores$Fecha)

# Renombrar la columna 'ds' a 'Fecha'
names(datos_estacionalidad_huevos)[names(datos_estacionalidad_huevos) == "ds"] <- "Fecha"
datos_estacionalidad_huevos$Fecha <- as.Date(datos_estacionalidad_huevos$Fecha)

# Fusionar las tablas por 'Fecha'
datos_merged_huevos <- merge(datos_huevos, datos_ipc[, c("Fecha", "Anual(en%)")], by = "Fecha", suffixes = c("", "_ipc"))
datos_merged_huevos <- merge(datos_merged_huevos, datos_paro[, c("Fecha", "TOTAL")], by = "Fecha", suffixes = c("", "_paro"))
datos_merged_huevos <- merge(datos_merged_huevos, datos_estacionalidad_huevos[, c("Fecha", "yearly")], by = "Fecha", suffixes = c("", "_estacionalidad"))
datos_merged_huevos <- merge(datos_merged_huevos, datos_estacionalidad_huevos[, c("Fecha", "trend")], by = "Fecha", suffixes = c("", "_estacionalidad"))
datos_merged_huevos <- merge(datos_merged_huevos, datos_regresores[, c("Fecha", "lockdown1")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_huevos <- merge(datos_merged_huevos, datos_regresores[, c("Fecha", "lockdown2")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_huevos <- merge(datos_merged_huevos, datos_regresores[, c("Fecha", "lockdown3")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_huevos <- merge(datos_merged_huevos, datos_regresores[, c("Fecha", "lockdown4")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_huevos <- merge(datos_merged_huevos, datos_regresores[, c("Fecha", "lockdown5")], by = "Fecha", suffixes = c("", "_regresores"))

# Verificar la estructura del data frame fusionado
str(datos_merged_huevos)
head(datos_merged_huevos)

cor(datos_merged_huevos[, c("CONSUMOXCAPITA", "GASTOXCAPITA", "PRECIOMEDIOkg", "PENETRACION(%)", "VALOR(MilesEuros)", "VOLUMEN(MilesKg)", "Anual(en%)", "TOTAL", "yearly", "trend", "lockdown1", "lockdown2", "lockdown3", "lockdown4", "lockdown5")])

# Ajustar el modelo de regresión lineal
modelo1_huevos <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + GASTOXCAPITA + `PENETRACION(%)` + `VOLUMEN(MilesKg)` + `Anual(en%)` + TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_huevos)
# Resumen del modelo
summary(modelo1_huevos)

# Ajustar el modelo de regresión lineal
modelo2_huevos <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + TOTAL + `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_huevos)
# Resumen del modelo
summary(modelo2_huevos)

# Ajustar el modelo de regresión lineal
modelo3_huevos <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_huevos)
# Resumen del modelo
summary(modelo3_huevos)

# Ajustar el modelo de regresión lineal
modelo4_huevos <- lm(CONSUMOXCAPITA ~ `Anual(en%)` + TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_huevos)
# Resumen del modelo
summary(modelo4_huevos)

# Ajustar el modelo de regresión lineal
modelo5_huevos <- lm(CONSUMOXCAPITA ~ `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3, data = datos_merged_huevos)
# Resumen del modelo
summary(modelo5_huevos)

# Ajustar el modelo de regresión lineal
modelo6_huevos <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + yearly + lockdown1 + lockdown2 + lockdown3, data = datos_merged_huevos)
# Resumen del modelo
summary(modelo6_huevos)

# Ajustar el modelo de regresión lineal
modelo7_huevos <- lm(CONSUMOXCAPITA ~ TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3 , data = datos_merged_huevos)
# Resumen del modelo
summary(modelo7_huevos)

vif(modelo1_huevos)
vif(modelo2_huevos)
vif(modelo3_huevos)
vif(modelo4_huevos)
vif(modelo5_huevos)
vif(modelo6_huevos)
vif(modelo7_huevos)


#COMPARATIVA datos originales vs Modelo2 predicho
fitted.values(modelo5_huevos)
plot(fitted.values(modelo5_huevos), type='l')

lines(datos_merged_huevos$CONSUMOXCAPITA, col='red')
summary(modelo5_huevos)
plot(fitted.values(modelo5_huevos), type='l', ylim=c(8, max(datos_merged_huevos$CONSUMOXCAPITA)),main="Datos originales VS valores ajustados con el modelo",
     xlab="Tiempo", ylab="CONSUMOXCAPITA (unds)")

#2.1 Análisis de los residuales de modelos lineales modelo6_huevos

#Modelado
modelo5_huevos <- lm(CONSUMOXCAPITA ~ `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3, data = datos_merged_huevos)
# Resumen del modelo
summary(modelo5_huevos)

#Análisis de los residuales
par(mfrow = c(2, 2))
plot(modelo5_huevos$residuals,main="Dispersión de residuos")
plot(modelo5_huevos$residuals ~ modelo5_huevos$fitted.values, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
#histograma de los residuos para comprobar su normalidad
hist(modelo5_huevos$residuals, main="Histograma de residuales", xlab="Residuales")
#Gráfico NOrmal Q-Q
qqnorm(modelo5_huevos$residuals)
qqline(modelo5_huevos$residuals)

#Comprobar normalidad
jarque.bera.test(modelo5_huevos$residuals)


#4 Pronosticos del modelo a 2 años
par(mfrow = c(1, 1))
modelo5_huevos <- lm(CONSUMOXCAPITA ~ `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3, data = datos_merged_huevos)

# obtener estacionalidad y tendencia
descomp_huevos <- stl(datos_huevos_ts, s.window = "period")
plot(descomp_huevos)
estacio_huevos <- descomp_huevos$time.series[,"seasonal"]
tende_huevos <- descomp_huevos$time.series[,"trend"]
datos_estacionalidad_huevos <- data.frame(forecast_huevos2)

# proyectar la estacionalidad repitiendo la estacionalidad del último año
x_estacio_huevos <- tail(datos_merged_huevos$yearly, 12)
plot(x_estacio_huevos)
# proyectar la tendencia con un suavizado exponencial
h_tend_huevos <- forecast::holt(y=tende_huevos, h = 12, damped = FALSE)

# Crear las variables predictoras para los próximos 2 años
nuevos_datos_huevos <- data.frame(
  `Anual(en%)` = c(rep(3.4, 13), rep(2.3, 11)),  # 3.4 para dic-24 y todo 2025, 2.3 para ene-nov 2026
  yearly = rep(x_estacio_huevos, length.out = 24),  # Usar la estacionalidad proyectada
  trend = rep(h_tend_huevos$mean, length.out = 24),  # Usar la tendencia proyectada con Holt-Winters
  lockdown1  = rep(0, 24),  # Asumimos que no habrá nuevos lockdowns
  lockdown2 = rep(0, 24),
  lockdown3 = rep(0, 24),
  lockdown4 = rep(0, 24),
  lockdown5 = rep(0, 24)
)

# Renombrar la columna 'Anual' a 'Anual(en%)' para que coincida con los nombres del modelo
names(nuevos_datos_huevos)[names(nuevos_datos_huevos) == "Anual.en.."] <- "Anual(en%)"

# Asignar fechas a los nuevos datos
nuevos_datos_huevos$Fecha <- seq(from = as.Date("2023-12-01"), by = "month", length.out = 24)

# Pronóstico con el modelo de regresión múltiple a 2 años
nuevos_datos_huevos$Prediccion_CONSUMOXCAPITA <- predict(modelo5_huevos, newdata = nuevos_datos_huevos)

# Unir con los datos históricos
datos_historicos_huevos <- datos_merged_huevos
datos_historicos_huevos$Prediccion_CONSUMOXCAPITA <- fitted(modelo5_huevos)

datos_historicos_huevos$Fecha <- as.Date(datos_historicos_huevos$Fecha)

datos_completos_huevos <- merge(datos_historicos_huevos, nuevos_datos_huevos[, c("Fecha", "Prediccion_CONSUMOXCAPITA")], by = "Fecha", all.x = TRUE, all.y = TRUE)
plot(datos_completos_huevos$Fecha,datos_completos_huevos$CONSUMOXCAPITA,  type = "l", col = "black", lwd = 1, ylim=c(9, max(datos_historicos_huevos$Prediccion_CONSUMOXCAPITA)),
     main = "Pronóstico regresión multivariante a 2 años", 
     xlab = "Fecha", ylab = "Consumo per cápita (Unds)")

lines(nuevos_datos_huevos$Fecha, nuevos_datos_huevos$Prediccion_CONSUMOXCAPITA, col = "red", lwd = 1, lty = 1)
legend("topright", legend = c("Histórico", "Pronóstico"), col = c("black", "red"), lwd = 1, cex = 0.95)


