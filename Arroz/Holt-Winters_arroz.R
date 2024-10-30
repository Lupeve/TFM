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

consulta_arroz <- "SELECT * FROM 'Tabla Arroz'"
datos_arroz <- dbGetQuery(con, consulta_arroz)

# Cerrar la conexión
dbDisconnect(con)

# Convertir los datos a un objeto ts (time series)
datos_arroz_ts <- ts(datos_arroz$CONSUMOXCAPITA, start = c(1999, 1), frequency = 12)

# Verificar la estructura de data_ts
str(datos_arroz_ts)

# modelo
moledo_arroz <- HoltWinters(datos_arroz_ts)

# serie historica estimada
estimado_arroz <- fitted(moledo_arroz)[,"xhat"]

# pronostico a 12 meses
pronostico_arroz <- predict(moledo_arroz, n.ahead=24)

length(datos_arroz_ts) #299
length(estimado_arroz) #287
length(pronostico_arroz) #24

# data.frame para la visualización
periodo.completo <- seq(as.Date("1999-01-01"),as.Date("2025-11-01"),by="month")
length(periodo.completo) #299
foo_arroz <-  data.frame(
  "tiempo"=periodo.completo,
  "dato.real"=c(datos_arroz_ts,rep(NA,24)),
  "estimado"=c(rep(NA,12),estimado_arroz,rep(NA,24)),
  "pronostico"=c(rep(NA,length(datos_arroz_ts)),pronostico_arroz)
)

matplot(foo_arroz[,2:4], type='l', lty=1, main="Pronóstico Holt-Winters a 2 años", xlab='', ylab='CONSUMOXCAPITA (kg)', xaxt='n',col=c('black','red','green'))
axis(side=1, at=1:length(foo_arroz$tiempo), labels = foo_arroz$tiempo, las=2, cex.axis=0.7)
legend("topright",legend=names(foo_arroz)[2:4], lty=1, col=c('black','red','green'))