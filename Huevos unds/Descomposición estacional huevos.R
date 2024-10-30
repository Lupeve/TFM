library(RSQLite)
library(forecast)
library(ggplot2)
library(tseries)
library(DBI)
library(foreign)
library(psych)
library(MASS)
library(dplyr)
library(corrplot)
library(prophet)

#conexion a la base de datos
ruta_bd <- "C:\\Users\\a1682\\Desktop\\Luis\\Luis\\TFM desarrollo\\Base de datos\\Tablas.db"
con <- dbConnect(RSQLite::SQLite(), dbname = ruta_bd)
consulta_huevos <- "SELECT * FROM 'Tabla Huevos unds'"
consulta_regresores <- "SELECT * FROM 'Tabla_prophet_2'"
datos_huevos <- dbGetQuery(con, consulta_huevos)
datos_regresores <- dbGetQuery(con, consulta_regresores)

# Cerrar la conexión
dbDisconnect(con)

# Convertir la columna 'Fecha' a tipo Date
datos_huevos$Fecha <- as.Date(datos_huevos$Fecha)
datos_regresores$Fecha <- as.Date(datos_regresores$Fecha)

# Seleccionar columnas específicas
datos2 <- datos_huevos[c("Fecha", "CONSUMOXCAPITA")]
# Renombrar columnas para Prophet
datos2 <- datos2 %>% rename(ds = Fecha, y = CONSUMOXCAPITA)

# 1) Descomposición estacional clásica por medias móviles
# Convertir los datos a un objeto ts (time series)
datos_huevos_ts <- ts(datos2$y, start = c(1999, 1), frequency = 12)
decomp_huevos <- decompose(datos_huevos_ts, type = "multiplicative")
plot(decomp_huevos)
# Extraer y graficar la estacionalidad
estacionalidad_huevos <- decomp_huevos$seasonal
plot(estacionalidad_huevos)

# Graficar la serie temporal
boxplot(datos_huevos_ts ~ cycle(datos_huevos_ts), ylab = "CONSUMOXCAPITA (unds)",xlab = "Mes")
plot(datos_huevos_ts, ylab="CONSUMOXCAPITA (unds)",xlab = "Año", main="Consumo per cápita huevos")

par(mar = c(6, 4, 4, 8), xpd = TRUE)
seasonplot(datos_huevos_ts, col=rainbow(12), year.labels = FALSE, ylab="CONSUMOXCAPITA (Unds)",xlab = "Mes", main = "")
# Crear un vector de los años correspondientes
años1 <- unique(floor(time(datos_huevos_ts)))
# Agregar la leyenda
legend("topright", inset = c(-0.25, 0), legend = años1, col = rainbow(12), 
       lty = 1, title = "Años", bty = "n", cex = 0.7)

carne_decomp<- decompose(datos_huevos_ts, type = "multiplicative")
par(mfrow = c(1, 1))  
par(mar = c(5, 4, 4, 2) + 0.1) 

# 2) Descomposición estacional por Loess
stl(datos_huevos_ts, s.window = "periodic") |> plot()
loess_huevos <- stl(datos_huevos_ts, s.window = "periodic") #|> plot()

# 3) Prophet sin regresores lineales
m_huevos <- prophet(seasonality.mode = "multiplicative")
m_huevos <- fit.prophet(m = m_huevos, df = datos2)
forecast_huevos <- predict(object = m_huevos)
plot(x = m_huevos, fcst = forecast_huevos, ylab="CONSUMOXCAPITA (Unds)",xlab = "Año", main = "")

prophet_plot_components(m = m_huevos, fcst = forecast_huevos)

# 4) Prophet con varios regresores lineales adicionales
datos2[,"lockdown1"] <- datos_regresores$lockdown1
datos2[,"lockdown2"] <- datos_regresores$lockdown2
datos2[,"lockdown3"] <- datos_regresores$lockdown3
datos2[,"lockdown4"] <- datos_regresores$lockdown4
datos2[,"lockdown5"] <- datos_regresores$lockdown5
m_huevos <- prophet(seasonality.mode="multiplicative")
m_huevos <- add_regressor(m_huevos, name='lockdown1')
m_huevos <- add_regressor(m_huevos, name='lockdown2')
m_huevos <- add_regressor(m_huevos, name='lockdown3')
m_huevos <- add_regressor(m_huevos, name='lockdown4')
m_huevos <- add_regressor(m_huevos, name='lockdown5')
m_huevos <- fit.prophet(m=m_huevos, df=datos2)
forecast_huevos2 <- predict(object=m_huevos)
plot(x=m_huevos, fcst=forecast_huevos2, ylab="CONSUMOXCAPITA (Unds)",xlab = "Año", main = "")

prophet_plot_components(m=m_huevos, fcst=forecast_huevos2)

#PRONOSTICO PROPHET
future2 <- make_future_dataframe(m_huevos, periods = 24, freq = 'month')  # 24 meses adicionales

#1.Añadir los regresores para el periodo de pronóstico
# Tendrás que especificar cómo los regresores se comportarán en el futuro
future2$lockdown1 <- c(datos2$lockdown1, rep(0, 24))  # Aquí se asume que lockdown1 no afectará en los próximos 2 años
future2$lockdown2 <- c(datos2$lockdown2, rep(0, 24))  # Ajusta los valores según lo que esperes que suceda
future2$lockdown3 <- c(datos2$lockdown3, rep(0, 24))
future2$lockdown4 <- c(datos2$lockdown4, rep(0, 24))
future2$lockdown5 <- c(datos2$lockdown5, rep(0, 24))

#2. Generar las predicciones
forecast_huevos2 <- predict(m_huevos, future2)

#3. Graficar los resultados
plot(m_huevos, forecast_huevos2, xlab = "Fecha", ylab = "CONSUMOXCAPITA (unds)", main = "Pronóstico de Consumo per cápita")

#4. Graficar los componentes (tendencia, estacionalidad, regresores)
prophet_plot_components(m_huevos, forecast_huevos2)
