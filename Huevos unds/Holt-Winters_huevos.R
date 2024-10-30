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

consulta_huevos <- "SELECT * FROM 'Tabla Huevos unds'"
datos_huevos <- dbGetQuery(con, consulta_huevos)

# Cerrar la conexión
dbDisconnect(con)

# Convertir los datos a un objeto ts (time series)
datos_huevos_ts <- ts(datos_huevos$CONSUMOXCAPITA, start = c(1999, 1), frequency = 12)

# Verificar la estructura de data_ts
str(datos_huevos_ts)

# modelo
modelo_huevos <- HoltWinters(datos_huevos_ts)

# serie historica estimada
estimado_huevos <- fitted(modelo_huevos)[,"xhat"]

# pronostico a 12 meses
pronostico_huevos <- predict(modelo_huevos, n.ahead=24)

length(datos_huevos_ts) #299
length(estimado_huevos) #287
length(pronostico_huevos) #24

# data.frame para la visualización
periodo.completo <- seq(as.Date("1999-01-01"),as.Date("2025-11-01"),by="month")
length(periodo.completo) #299
foo_huevos <-  data.frame(
  "tiempo"=periodo.completo,
  "dato.real"=c(datos_huevos_ts,rep(NA,24)),
  "estimado"=c(rep(NA,12),estimado_huevos,rep(NA,24)),
  "pronostico"=c(rep(NA,length(datos_huevos_ts)),pronostico_huevos)
)

matplot(foo_huevos[,2:4], type='l', lty=1, main="Pronóstico Holt-Winters a 2 años", xlab='', ylab='CONSUMOXCAPITA (unds)', xaxt='n',col=c('black','red','green'))
axis(side=1, at=1:length(foo_huevos$tiempo), labels = foo_huevos$tiempo, las=2, cex.axis=0.7)
legend("topright",legend=names(foo_huevos)[2:4], lty=1, col=c('black','red','green'))
