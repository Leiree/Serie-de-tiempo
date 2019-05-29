

# https://rpubs.com/palominoM/series
# https://www.statmethods.net/advstats/timeseries.html
# https://rpubs.com/joser/SeriesTemporalesBasicas

##############################################################################
### Librerias necesarias
############################################################################

library(forecast)
library(tseries)

##############################################################################
### Cargar los datos
############################################################################
setwd("~/Leire/SAVIA Leire/3ª Fase-Redes Neuronales/Datos R");
library(readr)
datos_panel <- read_csv("datos_panel_6_anios_2_setpoints.csv")
serie2 <- datos_panel[1:50000,]
dim(serie2) # 500 x 14

##############################################################################
### Analisis de datos
############################################################################

# 0. Transformar los datos en una serie temporal
# 1. Graficarla
# 2. Comprobar que sea estacionaria
# 3. Hacer transformacion
# 4. Definir el numero de parametros necesarios para arima
# 5. Crear modelo
# 6. Compara modelos y selecionar el mejor
# 7. Residuos
# 8. Predicion

##############################################################################
### 0. Transformar los datos en una serie temporal
############################################################################
serie <- ts(serie2$Temp, frequency = 144)

##############################################################################
### 1. Graficarla
############################################################################
library(ggfortify)
autoplot(serie, ts.colour = "blue", ts.linetype = "dashed")


##############################################################################
### 2. Comprobar que sea estacionaria
############################################################################
autoplot(acf(serie, plot = FALSE))
# La serie no es estacionaria, puesto que, la grafica que observamos
# no disminuye de forma esponencial.


##############################################################################
### 3. Hacer transformacion
############################################################################
# Se desconpone la serie
autoplot(stl(serie, s.window = "periodic"), ts.colour = "blue")
ndiffs(serie)
nsdiffs(serie)
serie_diff <- autoplot(diff(serie), ts.linetype = "dashed", ts.colour = "darkmagenta")
serie_diff
# Con esto consegimos eliminar la tendencia

##############################################################################
### 4. Definir el numero de parametros necesarios para arima
############################################################################
autoplot(acf(diff(serie), plot = FALSE))
autoplot(pacf(diff(serie), plot = FALSE))

##############################################################################
### 5. Crear modelo
############################################################################
library(forecast)
# Por un lado se puede crear una funcion que nos diga cual es el mejor arima,
# pero, para mis datos es muy pesado
# auto.arima(serie, stepwise = FALSE, approximation = FALSE)
# Tambien podemos hacer varios modelos y compararlos.
arima1<- Arima(serie, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
# arima2<- Arima(serie, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=144))
#  arima3<- Arima(serie, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=144))
# arima4<- Arima(serie, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=144))
# arima5<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=144))
# arima6<- Arima(co2ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=144))
# arima7<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=144))

##############################################################################
### 6. Compara modelos y selecionar el mejor
############################################################################
# AIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)
AIC(arima1)
BIC(arima1)

##############################################################################
### 7. Residuos
############################################################################
autoplot(acf(arima1$residuals, plot = FALSE))
autoplot(pacf(arima1$residuals, plot = FALSE))
ggtsdiag(arima1)

##############################################################################
### 8. Predicion
############################################################################
forecast1<-forecast(arima1, level = c(95), h = 50)
autoplot(forecast1)


