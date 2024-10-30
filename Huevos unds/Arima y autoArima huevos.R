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

consulta_huevos <- "SELECT * FROM 'Tabla Huevos unds'"
datos_huevos <- dbGetQuery(con, consulta_huevos)

# Cerrar la conexión
dbDisconnect(con)

# Convertir los datos a un objeto ts (time series)
datos_huevos_ts <- ts(datos_huevos$CONSUMOXCAPITA, start = c(1999, 1), frequency = 12)

# Verificar la estructura de data_ts
str(datos_huevos_ts)

# Graficar la serie temporal
boxplot(datos_huevos_ts ~ cycle(datos_huevos_ts))
plot(datos_huevos_ts, ylab="CONSUMOXCAPITA", main="Consumo per Capita total huevos")
seasonplot(datos_huevos_ts, col=rainbow(12), year.labels = TRUE)

huevos_decomp<- decompose(datos_huevos_ts, type = "multiplicative")

#Función ACF y PACF
par(mfrow = c(2, 1))
Acf(datos_huevos_ts, main='Función de autocorrelación (ACF)', ylab="ACF")
Pacf(datos_huevos_ts, main='Función de autocorrelación parcial (PACF)', ylab="PACF")
# Realizar prueba de raíz unitaria
adf.test(datos_huevos_ts)
ndiffs(datos_huevos_ts)

#Normalmente, lo ideal es diferenciar 1 o 2 veces, intentamos primero con 1 diferencia (en niveles) y graficamos 
#Para observar el comportamiento, debería mostrar una media y varianza constante 
#(no tendencia ni volatilidad)
par(mfrow = c(1, 1))
datos_huevos_d1= diff(datos_huevos_ts, differences = 1)
plot(datos_huevos_d1, main="Serie original diferenciada", xlab="Año", ylab="CONSUMOXCAPITA (Unds)")
adf.test(datos_huevos_d1)

#La función diferenciada (ACF y PACF)
par(mfrow = c(2, 1))
Acf(datos_huevos_d1, main='Función de autocorrelación (ACF) diferenciado', ylab="ACF")
Pacf(datos_huevos_d1, main='Función de autocorrelación parcial (PACF) diferenciado', ylab="PACF")

#Eligiendo p y q
par(mfrow = c(1, 1))
Model1=Arima(datos_huevos_ts,order=c(1,1,1),seasonal = list(order = c(1,1,2), period = 12),method="ML")
Box.test(Model1$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model1$residuals)
Model2=Arima(datos_huevos_ts,order=c(1,1,2),seasonal = list(order = c(1,1,2), period = 12),method="ML")
Box.test(Model2$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model2$residuals)

AIC(Model1,Model2)
BIC(Model1,Model2)


#Para detectar de forma automática el mejor modelo, se puede usar la función auto.arima:
#Corremos la función auto.arima:
auto.arima(datos_huevos_ts)

Model1_huevos=Arima(datos_huevos_ts,order=c(1,0,3),seasonal = list(order = c(1,1,2), period = 12),method="ML")
tsdiag(Model1_huevos)
Box.test(Model1_huevos$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model1_huevos$residuals)

#Se hace pronpostico a 12 y 24 meses. Se elije el parametro c(95) para que sea a un nivel de confianza del 95%
par(mfrow = c(1, 1))
Pronostico1_huevos=forecast(Model1_huevos,level= c(95), h=12)
plot(Pronostico1_huevos,main="Pronóstico ARIMA a 1 año",xlab="Año", ylab="CONSUMOXCAPITA (unds)")

Pronostico2_huevos=forecast(Model1_huevos,level= c(95), h=24)
plot(Pronostico2_huevos, main="Pronóstico ARIMA a 2 años",xlab="Año", ylab="CONSUMOXCAPITA (unds)")
