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

consulta_carne <- "SELECT * FROM 'Tabla total carne'"
datos_carne <- dbGetQuery(con, consulta_carne)

# Cerrar la conexión
dbDisconnect(con)

# Convertir los datos a un objeto ts (time series)
datos_carne_ts <- ts(datos_carne$CONSUMOXCAPITA, start = c(1999, 1), frequency = 12)

# Verificar la estructura de data_ts
str(datos_carne_ts)

# Graficar la serie temporal
boxplot(datos_carne_ts ~ cycle(datos_carne_ts), ylab = "Consumo per cápita (kg)",xlab = "Mes")
plot(datos_carne_ts, ylab="CONSUMOXCAPITA (kg)",xlab = "Año", main="Consumo per cápita Carne")

par(mar = c(6, 4, 4, 8), xpd = TRUE)
seasonplot(datos_carne_ts, col=rainbow(12), year.labels = FALSE, ylab="CONSUMOXCAPITA (kg)",xlab = "Mes", main = "")
# Crear un vector de los años correspondientes
años <- unique(floor(time(datos_carne_ts)))
# Agregar la leyenda
legend("topright", inset = c(-0.25, 0), legend = años, col = rainbow(12), 
       lty = 1, title = "Años", bty = "n", cex = 0.7)

carne_decomp<- decompose(datos_carne_ts, type = "multiplicative")
par(mfrow = c(1, 1))  
par(mar = c(5, 4, 4, 2) + 0.1) 

#Función ACF y PACF
par(mfrow = c(2, 1))
Acf(datos_carne_ts, main='Función de autocorrelación (ACF)', ylab="ACF")
Pacf(datos_carne_ts, main='Función de autocorrelación parcial (PACF)', ylab="PACF")
# Realizar prueba de raíz unitaria
adf.test(datos_carne_ts)
ndiffs(datos_carne_ts)

#Diferenciación
par(mfrow = c(1, 1))
datos_carne_d1= diff(datos_carne_ts, differences = 1)
plot(datos_carne_d1, main="Serie original diferenciada", xlab="Año", ylab="CONSUMOXCAPITA (kg)")
adf.test(datos_carne_d1)

#La función diferenciada (ACF y PACF)
par(mfrow = c(2, 1))
Acf(datos_carne_d1, main='Función de autocorrelación (ACF) diferenciado', ylab="ACF")
Pacf(datos_carne_d1, main='Función de autocorrelación parcial (PACF) diferenciado', ylab="PACF")

#Eligiendo p y q
par(mfrow = c(1, 1))
Model1=Arima(datos_carne_ts,order=c(1,1,1),seasonal = list(order = c(1,1,1), period = 12),method="ML")
Box.test(Model1$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model1$residuals)
Model2=Arima(datos_carne_ts,order=c(1,1,4),seasonal = list(order = c(1,1,1), period = 12),method="ML")
Box.test(Model2$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model2$residuals)
Model3=Arima(datos_carne_ts,order=c(4,1,1),seasonal = list(order = c(1,1,1), period = 12),method="ML")
Box.test(Model3$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model3$residuals)
Model4=Arima(datos_carne_ts,order=c(4,1,4),seasonal = list(order = c(1,1,1), period = 12),method="ML")
Box.test(Model4$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model4$residuals)

AIC(Model1,Model2,Model3,Model4)
BIC(Model1,Model2,Model3,Model4)

#Para detectar de forma automática el mejor modelo, se puede usar la función auto.arima:
#Corremos la función auto.arima:
auto.arima(datos_carne_ts)
Model1_carne=Arima(datos_carne_ts,order=c(0,1,4),seasonal = list(order = c(1,1,1), period = 12),method="ML")
tsdiag(Model1_carne)
Box.test(Model1_carne$residuals, lag = 24, type = "Ljung-Box")
jarque.bera.test(Model1_carne$residuals)

#Se hace pronpostico a 12 y 24 meses. Se elije el parametro c(95) para que sea a un nivel de confianza del 95%
par(mfrow = c(1, 1))
Pronostico1_carne=forecast(Model1_carne,level= c(95), h=12)
plot(Pronostico1_carne,main="Pronóstico ARIMA a 2 años",xlab="Año", ylab="CONSUMOXCAPITA (kg)")

Pronostico2_carne=forecast(Model1_carne,level= c(95), h=24)
plot(Pronostico2_carne,main="Pronóstico ARIMA a 2 años",xlab="Año", ylab="CONSUMOXCAPITA (kg)")