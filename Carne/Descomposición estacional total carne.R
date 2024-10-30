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
consulta_carne <- "SELECT * FROM 'Tabla total carne'"
consulta_regresores <- "SELECT * FROM 'Tabla_prophet_2'"
datos_carne <- dbGetQuery(con, consulta_carne)
datos_regresores <- dbGetQuery(con, consulta_regresores)

# Cerrar la conexión
dbDisconnect(con)

# Convertir la columna 'Fecha' a tipo Date
datos_carne$Fecha <- as.Date(datos_carne$Fecha)
datos_regresores$Fecha <- as.Date(datos_regresores$Fecha)

# Seleccionar columnas específicas
datos1 <- datos_carne[c("Fecha", "CONSUMOXCAPITA")]
# Renombrar columnas para Prophet
datos1 <- datos1 %>% rename(ds = Fecha, y = CONSUMOXCAPITA)

# 1) Descomposición estacional clásica por medias móviles
# Convertir los datos a un objeto ts (time series)
datos_carne_ts <- ts(datos1$y, start = c(1999, 1), frequency = 12)
decomp_carne <- decompose(datos_carne_ts, type = "multiplicative")
plot(decomp_carne)
# Extraer y graficar la estacionalidad
estacionalidad_carne <- decomp_carne$seasonal
plot(estacionalidad_carne)

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

# 2) Descomposición estacional por Loess
stl(datos_carne_ts, s.window = "periodic") |> plot()
#loess_carne <- stl(datos_carne_ts, s.window = "periodic") #|> plot()

# 3) Prophet sin regresores lineales
m_carne <- prophet(seasonality.mode = "multiplicative")
m_carne <- fit.prophet(m = m_carne, df = datos1)
forecast_carne <- predict(object = m_carne)
plot(x = m_carne, fcst = forecast_carne, ylab="CONSUMOXCAPITA (kg)",xlab = "Año", main = "")

prophet_plot_components(m = m_carne, fcst = forecast_carne)

# 4) Prophet con varios regresores lineales adicionales
datos1[,"lockdown1"] <- datos_regresores$lockdown1
datos1[,"lockdown2"] <- datos_regresores$lockdown2
datos1[,"lockdown3"] <- datos_regresores$lockdown3
datos1[,"lockdown4"] <- datos_regresores$lockdown4
datos1[,"lockdown5"] <- datos_regresores$lockdown5
m_carne <- prophet(seasonality.mode="multiplicative")
m_carne <- add_regressor(m_carne, name='lockdown1')
m_carne <- add_regressor(m_carne, name='lockdown2')
m_carne <- add_regressor(m_carne, name='lockdown3')
m_carne <- add_regressor(m_carne, name='lockdown4')
m_carne <- add_regressor(m_carne, name='lockdown5')
m_carne <- fit.prophet(m=m_carne, df=datos1)
forecast_carne2 <- predict(object=m_carne)
plot(x=m_carne, fcst=forecast_carne2, ylab="CONSUMOXCAPITA (kg)",xlab = "Año", main = "")

prophet_plot_components(m=m_carne, fcst=forecast_carne2)

#PRONOSTICO PROPHET
future <- make_future_dataframe(m_carne, periods = 24, freq = 'month')  # 24 meses adicionales

#1.Añadir los regresores para el periodo de pronóstico
# Tendrás que especificar cómo los regresores se comportarán en el futuro
future$lockdown1 <- c(datos1$lockdown1, rep(0, 24))  # Aquí se asume que lockdown1 no afectará en los próximos 2 años
future$lockdown2 <- c(datos1$lockdown2, rep(0, 24))  # Ajusta los valores según lo que esperes que suceda
future$lockdown3 <- c(datos1$lockdown3, rep(0, 24))
future$lockdown4 <- c(datos1$lockdown4, rep(0, 24))
future$lockdown5 <- c(datos1$lockdown5, rep(0, 24))

#2. Generar las predicciones
forecast_carne2 <- predict(m_carne, future)

#3. Graficar los resultados
plot(m_carne, forecast_carne2,xlab = "Fecha", ylab = "CONSUMOXCAPITA (kg)", main = "Pronóstico de Consumo per cápita")

#4. Graficar los componentes (tendencia, estacionalidad, regresores)
prophet_plot_components(m_carne, forecast_carne2)
