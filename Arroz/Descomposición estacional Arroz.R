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
consulta_arroz <- "SELECT * FROM 'Tabla Arroz'"
consulta_regresores <- "SELECT * FROM 'Tabla_prophet_2'"
datos_arroz <- dbGetQuery(con, consulta_arroz)
datos_regresores <- dbGetQuery(con, consulta_regresores)

# Cerrar la conexión
dbDisconnect(con)

# Convertir la columna 'Fecha' a tipo Date
datos_arroz$Fecha <- as.Date(datos_arroz$Fecha)
datos_regresores$Fecha <- as.Date(datos_regresores$Fecha)

# Seleccionar columnas específicas
datos3 <- datos_arroz[c("Fecha", "CONSUMOXCAPITA")]
# Renombrar columnas para Prophet
datos3 <- datos3 %>% rename(ds = Fecha, y = CONSUMOXCAPITA)

# 1) Descomposición estacional clásica por medias móviles
# Convertir los datos a un objeto ts (time series)
datos_arroz_ts <- ts(datos3$y, start = c(1999, 1), frequency = 12)
decomp_arroz <- decompose(datos_arroz_ts, type = "multiplicative")
plot(decomp_arroz)
# Extraer y graficar la estacionalidad
estacionalidad_arroz <- decomp_arroz$seasonal
plot(estacionalidad_arroz)

# Graficar la serie temporal
boxplot(datos_arroz_ts ~ cycle(datos_arroz_ts), ylab = "CONSUMOXCAPITA (kg)",xlab = "Mes")
plot(datos_arroz_ts, ylab="CONSUMOXCAPITA",xlab = "Año", main="Consumo per cápita Arroz")

par(mar = c(6, 4, 4, 8), xpd = TRUE)
seasonplot(datos_arroz_ts, col=rainbow(12), year.labels = FALSE, ylab="CONSUMOXCAPITA (kg)",xlab = "Mes", main = "")
# Crear un vector de los años correspondientes
años2 <- unique(floor(time(datos_arroz_ts)))
# Agregar la leyenda
legend("topright", inset = c(-0.25, 0), legend = años2, col = rainbow(12), 
       lty = 1, title = "Años", bty = "n", cex = 0.7)

carne_decomp<- decompose(datos_arroz_ts, type = "multiplicative")
par(mfrow = c(1, 1))  
par(mar = c(5, 4, 4, 2) + 0.1) 

# 2) Descomposición estacional por Loess
stl(datos_arroz_ts, s.window = "periodic") |> plot()
#loess_arroz <- stl(datos_arroz_ts, s.window = "periodic") #|> plot()

# 3) Prophet sin regresores lineales
m_arroz <- prophet(seasonality.mode = "multiplicative")
m_arroz <- fit.prophet(m = m_arroz, df = datos3)
forecast_arroz <- predict(object = m_arroz)
plot(x = m_arroz, fcst = forecast_arroz, ylab="CONSUMOXCAPITA (kg)",xlab = "Año", main = "")

prophet_plot_components(m = m_arroz, fcst = forecast_arroz)

# 4) Prophet con varios regresores lineales adicionales
datos3[,"lockdown1"] <- datos_regresores$lockdown1
datos3[,"lockdown2"] <- datos_regresores$lockdown2
datos3[,"lockdown3"] <- datos_regresores$lockdown3
datos3[,"lockdown4"] <- datos_regresores$lockdown4
datos3[,"lockdown5"] <- datos_regresores$lockdown5
m_arroz <- prophet(seasonality.mode="multiplicative")
m_arroz <- add_regressor(m_arroz, name='lockdown1')
m_arroz <- add_regressor(m_arroz, name='lockdown2')
m_arroz <- add_regressor(m_arroz, name='lockdown3')
m_arroz <- add_regressor(m_arroz, name='lockdown4')
m_arroz <- add_regressor(m_arroz, name='lockdown5')
m_arroz <- fit.prophet(m=m_arroz, df=datos3)
forecast_arroz2 <- predict(object=m_arroz)
plot(x=m_arroz, fcst=forecast_arroz2, ylab="CONSUMOXCAPITA (kg)",xlab = "Año", main = "")

prophet_plot_components(m=m_arroz, fcst=forecast_arroz2)

#PRONOSTICO PROPHET
future3 <- make_future_dataframe(m_arroz, periods = 24, freq = 'month')  # 24 meses adicionales

#1.Añadir los regresores para el periodo de pronóstico
# Tendrás que especificar cómo los regresores se comportarán en el futuro
future3$lockdown1 <- c(datos3$lockdown1, rep(0, 24))  # Aquí se asume que lockdown1 no afectará en los próximos 2 años
future3$lockdown2 <- c(datos3$lockdown2, rep(0, 24))  # Ajusta los valores según lo que esperes que suceda
future3$lockdown3 <- c(datos3$lockdown3, rep(0, 24))
future3$lockdown4 <- c(datos3$lockdown4, rep(0, 24))
future3$lockdown5 <- c(datos3$lockdown5, rep(0, 24))

#2. Generar las predicciones
forecast_arroz2 <- predict(m_arroz, future3)

#3. Graficar los resultados
plot(m_arroz, forecast_arroz2, xlab = "Fecha", ylab = "CONSUMOXCAPITA (kg)", main = "Pronóstico de Consumo per cápita")

#4. Graficar los componentes (tendencia, estacionalidad, regresores)
prophet_plot_components(m_arroz, forecast_arroz2)
