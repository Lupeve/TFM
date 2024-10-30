library(RSQLite)
library(forecast)
library(ggplot2)
library(tseries)
library(TSA)
library(DBI)
library(foreign)
library(psych)
library(MASS)
library(corrplot)
library(dplyr)
library(stats)
library(seasonal)

#conexion a la base de datos
ruta_bd <- "C:\\Users\\a1682\\Desktop\\Luis\\Luis\\TFM desarrollo\\Base de datos\\Tablas.db"
con <- dbConnect(RSQLite::SQLite(), dbname = ruta_bd)

consulta_arroz <- "SELECT * FROM 'Tabla Arroz'"
datos_arroz <- dbGetQuery(con, consulta_arroz)

# Cerrar la conexión
dbDisconnect(con)

# Convertir los datos a un objeto ts (time series)
datos_arroz_ts <- ts(datos_arroz$CONSUMOXCAPITA, start = c(1999, 1), frequency = 12)

# Verificar la estructura de data_ts
str(datos_arroz_ts)

# Graficar la serie temporal
boxplot(datos_arroz_ts ~ cycle(datos_arroz_ts))
plot(datos_arroz_ts, ylab="CONSUMOXCAPITA", main="Consumo per Capita Arroz")
seasonplot(datos_arroz_ts, col=rainbow(12), year.labels = TRUE)

#Función ACF y PACF
par(mfrow = c(2, 1))
Acf(datos_arroz_ts, main='Función de autocorrelación (ACF)', ylab="ACF")
Pacf(datos_arroz_ts, main='Función de autocorrelación parcial (PACF)', ylab="PACF")
# Realizar prueba de raíz unitaria
adf.test(datos_arroz_ts)
ndiffs(datos_arroz_ts)

#Diferenciación
par(mfrow = c(1, 1))
datos_arroz_d1= diff(datos_arroz_ts, differences = 1)
plot(datos_arroz_d1, main="Serie original diferenciada", xlab="Año", ylab="CONSUMOXCAPITA (kg)")
adf.test(datos_arroz_d1)

#La función diferenciada (ACF y PACF)
par(mfrow = c(2, 1))
Acf(datos_arroz_d1, main='Función de autocorrelación (ACF) diferenciado', ylab="ACF")
Pacf(datos_arroz_d1, main='Función de autocorrelación parcial (PACF) diferenciado', ylab="PACF")

#Eligiendo p y q
par(mfrow = c(1, 1))
Model1=Arima(datos_arroz_ts,order=c(2,1,1),seasonal = list(order = c(2,1,2), period = 12),method="ML")
Box.test(Model1$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model1$residuals)

AIC(Model1)
BIC(Model1)

#Para detectar de forma automática el mejor modelo, se puede usar la función auto.arima:
#Corremos la función auto.arima:
auto.arima(datos_arroz_ts)

Model1_arroz=Arima(datos_arroz_ts,order=c(1,0,2),seasonal = list(order = c(2,1,2), period = 12),method="ML")
tsdiag(Model1_arroz)
Box.test(Model1_arroz$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model1_arroz$residuals)

#Se hace pronpostico a 12 y 24 meses. Se elije el parametro c(95) para que sea a un nivel de confianza del 95%
par(mfrow = c(1, 1))
Pronostico1_arroz=forecast(Model1_arroz,level= c(95), h=12)
plot(Pronostico1_arroz, main="Pronóstico ARIMA a 1 años",xlab="Año", ylab="CONSUMOXCAPITA (kg)")

Pronostico2_arroz=forecast(Model1_arroz,level= c(95), h=24)
plot(Pronostico2_arroz, main="Pronóstico ARIMA a 2 años",xlab="Año", ylab="CONSUMOXCAPITA (kg)")