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
ruta_bd <- "C:\\Users\\a1682\\Desktop\\Luis\\Luis\\BBDD\\Tablas.db"
con <- dbConnect(RSQLite::SQLite(), dbname = ruta_bd)

consulta_alimentacion <- "SELECT * FROM 'Tabla Alimentacion total'"
datos_alimentacion <- dbGetQuery(con, consulta_alimentacion)

# Cerrar la conexión
dbDisconnect(con)


# Convertir los datos a un objeto ts (time series)
datos_alimentacion_ts <- ts(datos_alimentacion$CONSUMOXCAPITA, start = c(1999, 1), frequency = 12)

# Verificar la estructura de data_ts
str(datos_alimentacion_ts)

# modelo
m <- HoltWinters(datos_alimentacion_ts)

# serie historica estimada
estimado <- fitted(m)[,"xhat"]

# pronostico a 12 meses
pronostico <- predict(m, n.ahead=24)

length(datos_alimentacion_ts) #299
length(estimado) #287
length(pronostico) #24

# data.frame para la visualización
periodo.completo <- seq(as.Date("1999-01-01"),as.Date("2025-11-01"),by="month")
length(periodo.completo) #299
foo <-  data.frame(
    "tiempo"=periodo.completo,
    "dato.real"=c(datos_alimentacion_ts,rep(NA,24)),
    "estimado"=c(rep(NA,12),estimado,rep(NA,24)),
    "pronostico"=c(rep(NA,length(datos_alimentacion_ts)),pronostico)
  )

matplot(foo[,2:4], type='l', lty=1, main="Pronóstico Holt-Winters a 2 años", xlab='', ylab='CONSUMOXCAPITA (kg)', xaxt='n',col=c('black','red','green'))
axis(side=1, at=1:length(foo$tiempo), labels = foo$tiempo, las=2, cex.axis=0.7)
legend("topright",legend=names(foo)[2:4], lty=1, col=c('black','red','green'))
