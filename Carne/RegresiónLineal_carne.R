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
#Ejecutar primero Descomposicion estacional huevos (clásica, Loess y Prophet).R y seguidamente estos comandos:
#conexion a la base de datos
ruta_bd <- "C:\\Users\\a1682\\Desktop\\Luis\\Luis\\TFM desarrollo\\Base de datos\\Tablas.db"
con <- dbConnect(RSQLite::SQLite(), dbname = ruta_bd)

consulta_carne <- "SELECT * FROM 'Tabla total carne'"
consulta_ipc <- "SELECT * FROM 'Tabla IPC'"
consulta_paro <- "SELECT * FROM 'Paro_sexoyedad_total'"
consulta_regresores <- "SELECT * FROM 'Tabla_prophet_2'"

datos_carne <- dbGetQuery(con, consulta_carne)
datos_ipc <- dbGetQuery(con, consulta_ipc)
datos_paro <- dbGetQuery(con, consulta_paro)
datos_regresores <- dbGetQuery(con, consulta_regresores)

# Cerrar la conexión
dbDisconnect(con)

# Convertir la columna 'Fecha' a tipo Date
datos_carne$Fecha <- as.Date(datos_carne$Fecha)
datos_ipc$Fecha <- as.Date(datos_ipc$Fecha)
datos_paro$Fecha <- as.Date(datos_paro$Fecha)
datos_estacionalidad_carne <- data.frame(forecast_carne2)
datos_regresores$Fecha <- as.Date(datos_regresores$Fecha)

# Renombrar la columna 'ds' a 'Fecha'
names(datos_estacionalidad_carne)[names(datos_estacionalidad_carne) == "ds"] <- "Fecha"
datos_estacionalidad_carne$Fecha <- as.Date(datos_estacionalidad_carne$Fecha)

# Fusionar las tablas por 'Fecha'
datos_merged_carne <- merge(datos_carne, datos_ipc[, c("Fecha", "Anual(en%)")], by = "Fecha", suffixes = c("", "_ipc"))
datos_merged_carne <- merge(datos_merged_carne, datos_paro[, c("Fecha", "TOTAL")], by = "Fecha", suffixes = c("", "_paro"))
datos_merged_carne <- merge(datos_merged_carne, datos_estacionalidad_carne[, c("Fecha", "yearly")], by = "Fecha", suffixes = c("", "_estacionalidad"))
datos_merged_carne <- merge(datos_merged_carne, datos_estacionalidad_carne[, c("Fecha", "trend")], by = "Fecha", suffixes = c("", "_estacionalidad"))
datos_merged_carne <- merge(datos_merged_carne, datos_regresores[, c("Fecha", "lockdown1")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_carne <- merge(datos_merged_carne, datos_regresores[, c("Fecha", "lockdown2")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_carne <- merge(datos_merged_carne, datos_regresores[, c("Fecha", "lockdown3")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_carne <- merge(datos_merged_carne, datos_regresores[, c("Fecha", "lockdown4")], by = "Fecha", suffixes = c("", "_regresores"))
datos_merged_carne <- merge(datos_merged_carne, datos_regresores[, c("Fecha", "lockdown5")], by = "Fecha", suffixes = c("", "_regresores"))

# Verificar la estructura del data frame fusionado
str(datos_merged_carne)
head(datos_merged_carne)

cor(datos_merged_carne[, c("CONSUMOXCAPITA", "GASTOXCAPITA", "PRECIOMEDIOkg", "PENETRACION(%)", "VALOR(MilesEuros)", "VOLUMEN(MilesKg)", "Anual(en%)", "TOTAL", "yearly", "trend", "lockdown1", "lockdown2", "lockdown3", "lockdown4", "lockdown5")])

# Ajustar el modelo de regresión lineal
modelo1_carne <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + GASTOXCAPITA + `PENETRACION(%)` + `VOLUMEN(MilesKg)` + `Anual(en%)` + TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_carne)
# Resumen del modelo
summary(modelo1_carne)

# Ajustar el modelo de regresión lineal
modelo2_carne <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_carne)
# Resumen del modelo
summary(modelo2_carne)

# Ajustar el modelo de regresión lineal
modelo3_carne <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_carne)
# Resumen del modelo
summary(modelo3_carne)

# Ajustar el modelo de regresión lineal
modelo4_carne <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_carne)
# Resumen del modelo
summary(modelo4_carne)

# Ajustar el modelo de regresión lineal
modelo5_carne <- lm(CONSUMOXCAPITA ~ PRECIOMEDIOkg + `Anual(en%)` + TOTAL + yearly + trend + lockdown1 + lockdown2 + lockdown3+ lockdown4+ lockdown5, data = datos_merged_carne)
# Resumen del modelo
summary(modelo5_carne)

# Ajustar el modelo de regresión lineal
modelo6_carne <- lm(CONSUMOXCAPITA ~ `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3 + lockdown4+ lockdown5, data = datos_merged_carne)
# Resumen del modelo
summary(modelo6_carne)

# Ajustar el modelo de regresión lineal
modelo7_carne <- lm(CONSUMOXCAPITA ~ yearly + trend + lockdown1 + lockdown2 + lockdown3 + lockdown4 + lockdown5, data = datos_merged_carne)
# Resumen del modelo
summary(modelo7_carne)

vif(modelo1_carne)
vif(modelo2_carne)
vif(modelo3_carne)
vif(modelo4_carne)
vif(modelo5_carne)
vif(modelo6_carne)
vif(modelo7_carne)


#COMPARATIVA datos originales vs Modelo2 predicho
fitted.values(modelo6_carne)
plot(fitted.values(modelo6_carne), type='l')

lines(datos_merged_carne$CONSUMOXCAPITA, col='red')
summary(modelo6_carne)
plot(fitted.values(modelo6_carne), type='l', ylim=c(2, max(datos_merged_carne$CONSUMOXCAPITA)),main="Datos originales VS valores ajustados con el modelo",
     xlab="Tiempo", ylab="CONSUMOXCAPITA (kg)")

#2.1 Análisis de los residuales de modelos lineales modelo6_carne

#Modelado
modelo6_carne <- lm(CONSUMOXCAPITA ~ `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3 + lockdown4+ lockdown5, data = datos_merged_carne)
# Resumen del modelo
summary(modelo6_carne)

#Análisis de los residuales
par(mfrow = c(2, 2))
plot(modelo6_carne$residuals,main="Dispersión de residuos")
plot(modelo6_carne$residuals ~ modelo6_carne$fitted.values, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
#histograma de los residuos para comprobar su normalidad
hist(modelo6_carne$residuals, main="Histograma de residuales", xlab="Residuales")
#Gráfico NOrmal Q-Q
qqnorm(modelo6_carne$residuals)
qqline(modelo6_carne$residuals)

#Comprobar normalidad
jarque.bera.test(modelo6_carne$residuals)

#2.2 Mejorar la Normalidad de los Residuos con Log
modelo6_carne_log <- lm(log(CONSUMOXCAPITA) ~ `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3 + lockdown4 + lockdown5, data = datos_merged_carne)
summary(modelo6_carne_log)
#Análisis de los residuales
par(mfrow = c(2, 2))
plot(modelo6_carne_log$residuals,main="Dispersión de residuos")
plot(modelo6_carne_log$residuals ~ modelo6_carne_log$fitted.values, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
#histograma de los residuos para comprobar su normalidad
hist(modelo6_carne_log$residuals, main="Histograma de residuales", xlab="Residuales")
#Gráfico NOrmal Q-Q
qqnorm(modelo6_carne_log$residuals)
qqline(modelo6_carne_log$residuals)

#Comprobar normalidad
jarque.bera.test(modelo6_carne_log$residuals)

#COMPARATIVA datos originales vs Modelo6_carne_log predicho
par(mfrow = c(1, 1))
fitted_original2 <- exp(fitted.values(modelo6_carne_log))
plot(fitted_original2, type='l')

lines(datos_merged_carne$CONSUMOXCAPITA, col='red')
summary(modelo6_carne_log)
plot(fitted_original2, type='l', ylim=c(2, max(datos_merged_carne$CONSUMOXCAPITA)),main="Datos originales VS valores ajustados con el modelo",
     xlab="Tiempo", ylab="CONSUMOXCAPITA (kg)")

#2.3 Análisis de los residuales de modelos lineales modelo7_carne

#Análisis de los residuales
par(mfrow = c(2, 2))
plot(modelo7_carne$residuals,main="Dispersión de residuos")
plot(modelo7_carne$residuals ~ modelo7_carne$fitted.values, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
#histograma de los residuos para comprobar su normalidad
hist(modelo7_carne$residuals, main="Histograma de residuales", xlab="Residuales")
#Gráfico NOrmal Q-Q
qqnorm(modelo7_carne$residuals)
qqline(modelo7_carne$residuals)

#Comprobar normalidad
jarque.bera.test(modelo7_carne$residuals)

#2.4 Mejorar la Normalidad de los Residuos con Log
modelo7_carne_log <- lm(log(CONSUMOXCAPITA) ~ yearly + trend + lockdown1 + lockdown2 + lockdown3 + lockdown4 + lockdown5, data = datos_merged_carne)
summary(modelo7_carne_log)
#Análisis de los residuales
par(mfrow = c(2, 2))
plot(modelo7_carne_log$residuals,main="Dispersión de residuos")
plot(modelo7_carne_log$residuals ~ modelo7_carne_log$fitted.values, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
#histograma de los residuos para comprobar su normalidad
hist(modelo7_carne_log$residuals, main="Histograma de residuales", xlab="Residuales")
#Gráfico NOrmal Q-Q
qqnorm(modelo7_carne_log$residuals)
qqline(modelo7_carne_log$residuals)

#Comprobar normalidad
jarque.bera.test(modelo7_carne_log$residuals)

#COMPARATIVA datos originales vs Modelo6_carne_log predicho
par(mfrow = c(1, 1))
fitted_original2 <- exp(fitted.values(modelo6_carne_log))
plot(fitted_original2, type='l')

lines(datos_merged_carne$CONSUMOXCAPITA, col='red')
summary(modelo6_carne_log)
plot(fitted_original2, type='l', ylim=c(2, max(datos_merged_carne$CONSUMOXCAPITA)),main="Datos originales VS valores ajustados con el modelo",
     xlab="Tiempo", ylab="CONSUMOXCAPITA (kg)")


#4 Pronosticos del modelo a 2 años

modelo6_carne <- lm(CONSUMOXCAPITA ~ `Anual(en%)` + yearly + trend + lockdown1 + lockdown2 + lockdown3 + lockdown4+ lockdown5, data = datos_merged_carne)

# obtener estacionalidad y tendencia
descomp_carne <- stl(datos_carne_ts, s.window = "period")
plot(descomp_carne)
estacio_carne <- descomp_carne$time.series[,"seasonal"]
tende_carne <- descomp_carne$time.series[,"trend"]
datos_estacionalidad_carne <- data.frame(forecast_carne2)


# proyectar la estacionalidad repitiendo la estacionalidad del último año
x_estacio_carne <- tail(datos_merged_carne$yearly, 12)
plot(x_estacio_carne)
# proyectar la tendencia con un suavizado exponencial
h_tend_carne <- forecast::holt(y=tende_carne, h = 12, damped = FALSE)

# Crear las variables predictoras para los próximos 2 años
nuevos_datos_carne <- data.frame(
  `Anual(en%)` = c(rep(3.4, 13), rep(2.3, 11)),  # 3.4 para dic-24 y todo 2025, 2.3 para ene-nov 2026
  yearly = rep(x_estacio_carne, length.out = 24),  # Usar la estacionalidad proyectada
  trend = rep(h_tend_carne$mean, length.out = 24),  # Usar la tendencia proyectada con Holt-Winters
  lockdown1  = rep(0, 24),  # Asumimos que no habrá nuevos lockdowns
  lockdown2 = rep(0, 24),
  lockdown3 = rep(0, 24),
  lockdown4 = rep(0, 24),
  lockdown5 = rep(0, 24)
)

# Renombrar la columna 'Anual' a 'Anual(en%)' para que coincida con los nombres del modelo
names(nuevos_datos_carne)[names(nuevos_datos_carne) == "Anual.en.."] <- "Anual(en%)"

# Asignar fechas a los nuevos datos
nuevos_datos_carne$Fecha <- seq(from = as.Date("2023-12-01"), by = "month", length.out = 24)

# Pronóstico con el modelo de regresión múltiple a 2 años
nuevos_datos_carne$Prediccion_CONSUMOXCAPITA <- predict(modelo6_carne, newdata = nuevos_datos_carne)

# Unir con los datos históricos
datos_historicos_carne <- datos_merged_carne
datos_historicos_carne$Prediccion_CONSUMOXCAPITA <- fitted(modelo6_carne)

datos_historicos_carne$Fecha <- as.Date(datos_historicos_carne$Fecha)

datos_completos_carne <- merge(datos_historicos_carne, nuevos_datos_carne[, c("Fecha", "Prediccion_CONSUMOXCAPITA")], by = "Fecha", all.x = TRUE, all.y = TRUE)
plot(datos_completos_carne$Fecha,datos_completos_carne$CONSUMOXCAPITA,  type = "l", col = "black", lwd = 1, ylim=c(2.5, max(datos_historicos_carne$CONSUMOXCAPITA)),
     main = "Pronóstico regresión multivariante a 2 años", 
     xlab = "Fecha", ylab = "Consumo per cápita (kg)")

lines(nuevos_datos_carne$Fecha, nuevos_datos_carne$Prediccion_CONSUMOXCAPITA, col = "red", lwd = 1, lty = 1)
legend("topright", legend = c("Histórico", "Pronóstico"), col = c("black", "red"), lwd = 1)



