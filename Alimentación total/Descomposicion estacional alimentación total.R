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
consulta_alimentacion <- "SELECT * FROM 'Tabla_prophet_2'"
datos_alimentacion <- dbGetQuery(con, consulta_alimentacion)

# Cerrar la conexión
dbDisconnect(con)

# Convertir la columna 'Fecha' a tipo Date
datos_alimentacion$Fecha <- as.Date(datos_alimentacion$Fecha)

# Seleccionar columnas específicas
datos <- datos_alimentacion[c("Fecha", "CONSUMOXCAPITA")]
# Renombrar columnas para Prophet
datos <- datos %>% rename(ds = Fecha, y = CONSUMOXCAPITA)

# 1) Descomposición estacional clásica por medias móviles
# Convertir los datos a un objeto ts (time series)
datos_alimentacion_ts <- ts(datos$y, start = c(1999, 1), frequency = 12)
decompose(datos_alimentacion_ts, type = "multiplicative") |> plot()
# Extraer y graficar la estacionalidad
estacionalidad <- decomp$time.series[, "seasonal"]
plot(estacionalidad)

# 2) Descomposición estacional por Loess
stl(datos_alimentacion_ts, s.window = "periodic") |> plot()
#loess <- stl(datos_alimentacion_ts, s.window = "periodic") #|> plot()

# 3) Prophet sin regresores lineales
m <- prophet(seasonality.mode="multiplicative")
m <- fit.prophet(m=m, df=datos)
forecast <- predict(object=m)
plot(x = m, fcst = forecast, ylab="CONSUMOXCAPITA (kg)",xlab = "Año", main = "")

prophet_plot_components(m=m, fcst=forecast)

# 4) Prophet con varios regresores lineales adicionales
datos[,"lockdown1"] <- datos_alimentacion$lockdown1
datos[,"lockdown2"] <- datos_alimentacion$lockdown2
datos[,"lockdown3"] <- datos_alimentacion$lockdown3
datos[,"lockdown4"] <- datos_alimentacion$lockdown4
datos[,"lockdown5"] <- datos_alimentacion$lockdown5
m <- prophet(seasonality.mode="multiplicative")
m <- add_regressor(m, name='lockdown1')
m <- add_regressor(m, name='lockdown2')
m <- add_regressor(m, name='lockdown3')
m <- add_regressor(m, name='lockdown4')
m <- add_regressor(m, name='lockdown5')
m <- fit.prophet(m=m, df=datos)
forecast <- predict(object=m)
plot(x=m, fcst=forecast, ylab="CONSUMOXCAPITA (kg)",xlab = "Año", main = "")

prophet_plot_components(m=m, fcst=forecast)

#PRONOSTICO PROPHET
future_total <- make_future_dataframe(m, periods = 24, freq = 'month')  # 24 meses adicionales

#1.Añadir los regresores para el periodo de pronóstico
# Tendrás que especificar cómo los regresores se comportarán en el futuro
future_total$lockdown1 <- c(datos$lockdown1, rep(0, 24))  # Aquí se asume que lockdown1 no afectará en los próximos 2 años
future_total$lockdown2 <- c(datos$lockdown2, rep(0, 24))  # Ajusta los valores según lo que esperes que suceda
future_total$lockdown3 <- c(datos$lockdown3, rep(0, 24))
future_total$lockdown4 <- c(datos$lockdown4, rep(0, 24))
future_total$lockdown5 <- c(datos$lockdown5, rep(0, 24))

#2. Generar las predicciones
forecast <- predict(m, future_total)

#3. Graficar los resultados
plot(m, forecast, xlab = "Fecha", ylab = "CONSUMOXCAPITA (kg)", main = "Pronóstico de Consumo per cápita")

#4. Graficar los componentes (tendencia, estacionalidad, regresores)
prophet_plot_components(m, forecast)


