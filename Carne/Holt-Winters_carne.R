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

consulta_carne <- "SELECT * FROM 'Tabla total carne'"
datos_carne <- dbGetQuery(con, consulta_carne)

# Cerrar la conexión
dbDisconnect(con)

# Convertir los datos a un objeto ts (time series)
datos_carne_ts <- ts(datos_carne$CONSUMOXCAPITA, start = c(1999, 1), frequency = 12)

# Verificar la estructura de data_ts
str(datos_carne_ts)

# modelo
moledo_carne <- HoltWinters(datos_carne_ts)

# serie historica estimada
estimado_carne <- fitted(moledo_carne)[,"xhat"]

# pronostico a 12 meses
pronostico_carne <- predict(moledo_carne, n.ahead=24)

length(datos_carne_ts) #299
length(estimado_carne) #287
length(pronostico_carne) #24

# data.frame para la visualización
periodo.completo <- seq(as.Date("1999-01-01"),as.Date("2025-11-01"),by="month")
length(periodo.completo) #299
foo_carne <-  data.frame(
  "tiempo"=periodo.completo,
  "dato.real"=c(datos_carne_ts,rep(NA,24)),
  "estimado"=c(rep(NA,12),estimado_carne,rep(NA,24)),
  "pronostico"=c(rep(NA,length(datos_carne_ts)),pronostico_carne)
)

matplot(foo_carne[,2:4], type='l', lty=1,main="Pronóstico Holt-Winters a 2 años", xlab='', ylab='CONSUMOXCAPITA (kg)', xaxt='n',col=c('black','red','green'))
axis(side=1, at=1:length(foo_carne$tiempo), labels = foo_carne$tiempo, las=2, cex.axis=0.7)
legend("topright",legend=names(foo_carne)[2:4], lty=1, col=c('black','red','green'))