

#### Introduccion ####
# El siguiente estudio de series temporales en R consiste en una descomposicion de una serie y el
# estudio de su estacionalidad, la busqueda y testeo del modelo de suavizado exponencial mas adecuado para
# esta serie y, finalmente, trataremos de aplicar las transformaciones neceasarias para transformar nuestra serie
# en una serie estacionaria y encontrar el modelo más adecuado para replicar su comportamiento y asi poder
# generar predicciones.


getwd()
setwd('C:/Users/Javier/Desktop/Proyectos/Time_Series_Analisis_R')


#### Librerias a utilizar ####
library(readxl)
library(fpp2)
library(tseries)
library(forecast)
library(ggplot2)
library(seasonal)
library(descomponer)
library(TSA)

# Funciones definidas para simplificar
source("Funciones_R.R")


# Los datos que utilizaremos en el próximo análisis corresponden al valor del IPC mensual en España entre enero de
# 2002 y enero de 2020 (217 observaciones). Éstos han sido obtenidos del Instituto Nacionacional de Estadística (INE).


#### Importamos los datos ####
datos <- read_xls('IPC.xls', skip=7, n_max=218)

# Inviertimos el orden porque venian ordenados del mas actual al mas antiguo
datos <- datos[c(seq(217,1)),]

# Convertimos a un objeto serie temporal
ipc <- ts(datos[,2], start=c(2002,1), frequency=12)



#### EDA ####

# Representamos la serie
autoplot(ipc)+  ggtitle("IPC") +  xlab("Mes/Año") +  ylab("Indice")

# Descomposicion de la serie
ipc_Comp_Sum<- decompose(ipc,type=c("additive"))
ipc_Comp_Mult<- decompose(ipc,type=c("multiplicative"))

# Representacion de la descomposicion
autoplot(ipc_Comp_Sum)
autoplot(ipc_Comp_Mult)

# Coeficientes debidos a la estacionalidad
ipc_Comp_Sum$figure
ipc_Comp_Mult$figure

# Analisis grafico de la estacionalidad. Representacion por año
ggseasonplot(ipc, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("Numero") +  ggtitle("Seasonal plot: IPC mensual en Espa?a")


# Representacion de resiuos
autoplot(ipc_Comp_Sum$random)
mean(ipc_Comp_Sum$random, na.rm = T)
sd(ipc_Comp_Sum$random, na.rm = T)

autoplot(ipc_Comp_Mult$random)
mean(ipc_Comp_Mult$random, na.rm = T)
sd(ipc_Comp_Mult$random, na.rm = T)


# Construccion del periodograma 
gperiodograma(diff(log(ipc)))
gperiodograma(diff(diff(log(ipc)),36))
gperiodograma(diff(log(ipc),36))
gperiodograma(diff(log(ipc),38))



#### Division en Train y Test ####

# Seleccionamos toda la serie excepto los valores del ultimo año para ajustar los modelos (train set)
ipc_tr<-window(ipc,end=c(2018,12))

# Seleccionamos el ultimo año para comparar predicciones (test set)
ipc_tst<-window(ipc,start=c(2019,1))



#### Modelo 1 ####
### Suavizado exponencial simple ses(). Prediccion a un año
ipc_s1=ses(ipc_tr, h=12)

# Inspeccion del objeto: valores de las predicciones, coeficientes del modelo, inspeccion de los residuos
print(ipc_s1)
ipc_s1$model
autoplot(ipc_s1$residuals)

# Representamos los valores observados y los suavizados con la prediccion 
autoplot(ipc_s1) +
    autolayer(fitted(ipc_s1), series="Fitted") + autolayer(ipc_tst, series="actual") +
    ylab("Indice") + xlab("Mes/Año")


#### Modelo 2 ####
### Suavizado Exponencial doble de Holt
ipc_sh <- holt(ipc_tr, h=12)

# Inspeccion del objeto: valores de las predicciones, coeficientes del modelo, inspeccion de los residuos
print(ipc_sh)
ipc_sh$model
autoplot(ipc_sh$residuals)

# Representamos los valores observados y los suavizados con la prediccion 
autoplot(ipc_sh) +
    autolayer(fitted(ipc_sh), series="Fitted") +autolayer(ipc_tst, series="Actual") +
    ylab("Indice") + xlab("Mes/A?o")


#### Modelo 3 ####
### Suavizado Exponencial con estacionalidad. Holt-Winters
ipc_hw_mult <- hw(ipc_tr, seasonal='multiplicative', h=12, level = c(80, 95))
ipc_hw_add <- hw(ipc_tr, seasonal='additive', h=12, level = c(80, 95))

# Inspeccion del objeto: valores de las predicciones, coeficientes del modelo, inspeccion de los residuos
print(ipc_hw_mult)
ipc_hw_mult$model
autoplot(ipc_hw_mult$residuals)
checkresiduals(ipc_hw_mult)

# Inspeccion del objeto: valores de las predicciones, coeficientes del modelo, inspeccion de los residuos
print(ipc_hw_add)
ipc_hw1_add$model
autoplot(ipc_hw1_add$residuals)
checkresiduals(ipc_hw1_add)


# Representamos los valores observados y los suavizados con la prediccion 
autoplot(ipc_hw_mult) +
    autolayer(fitted(ipc_hw_mult), series="Fitted") + autolayer(ipc_tst, series="actual") +
    ylab("Indice") + xlab("Mes/Año")

autoplot(ipc_hw_add) +
    autolayer(fitted(ipc_hw_add), series="Fitted") + autolayer(ipc_tst, series="actual") +
    ylab("Indice") + xlab("Mes/Año")


#### Comprobamos la precision de las predicciones de los distintos modelos contemplados ####
accuracy(ipc_s1,ipc_tst)
accuracy(ipc_sh,ipc_tst)
accuracy(ipc_hw_mult,ipc_tst)
accuracy(ipc_hw_add,ipc_tst)


# Atendiendo al resultado obtenido para RMSE, y tal y como anticipaba el gráfico anterior, el modelo que mejor
# refleja el comportamiento de nuestros datos es el metodo de Suavizado Exponencial de Holt-Winters (aditivo).



# ======================================================= #


#### Busqueda del modelo ARIMA que mejor representa el comportamiento de nuestra serie ####


## Convertimos la serie a estacionaria ##

# Calculamos  las autocorrelaciones simples hasta el retardo 48
ggAcf(ipc, lag=48) # Decrecimineto lento--> No estacionaria-->diferenciar
# Calculamos  las autocorrelaciones parciales hasta el retardo 48
ggPacf(ipc, lag=48)

# Como podemos observar en la función de autocorrelación simple, el decrecimiento lento y lineal de la
# significatividad en los retardos y el excesivo número de ellos que son significativos indican que la serie
# no es estacionaria.


# Serie diferenciada: autocorrelaciones simples hasta el retardo 48
ggAcf(diff(ipc), lag=48) # No estacionaria
# Calculamos  las autocorrelaciones parciales hasta el retardo 48
ggPacf(diff(ipc), lag=48)

# Tras diferenciar la serie podemos ver que las autocorrelaciones simples y parciales se han visto altamente
# afectadas. Podemos observar la significatividad que presentan los retardos multiplos de 3 en la ACF, denotando,
# así como los primeros retardos significativos en la PACF. No obstante, la serie sigue sin ser estacionaria.
# La ACF nos hace pensar que podría existir algún tipo de estacionalidad trimestral o anual que ha de ser
# corregida mediante una diferenciacion del orden adecuado de la serie para que ésta sea estacionaria.


# Diferenciacion de retardo 12
ggAcf(diff(ipc, 12), lag=48)
ggPacf(diff(ipc, 12), lag=48)

# Diferenciacion de retardo 12 sobre la diff de orden 1
ggAcf(diff(diff(ipc),12), lag=48)
ggPacf(diff(diff(ipc),12), lag=48)

# Ahora podemos ver como nos hemos acercado claramente a la estacionalidad dados los retardos que son
# significativos tanto en la ACF como en la PACF (retardo -1 y multiplos de 12) y como la 
# significatividad de estos no persiste a medida que nos aumentamos los retardos.

# Parece que la parte no estacional es un ARMA puesto que corta tanto en el ACF como en el PACF. En cuanto a
# la parte estacional vemos que cada 12 meses hay un pico en el PACF que va decreciendo, lo que indica la
# existencia de un MA.
# ---> Candidato: Arima(1,1,1)(0,1,1)



# Test de adf para comprobar la existencia de raiz unitaria
adf.test(diff(ipc))
adf.test(diff(ipc, 12))
adf.test(diff(diff(ipc), 12))

# Este último es el que tiene el menor estadístico de DickeyFuller, indicando un mayor rechazo a la
# hipótesis nula de que existe una raizunitaria y, por tanto, la serie no es estacionaria. En pocas palabras,
# diff(diff(ipc), 12) es estacionaria.



#### Contruccion de los modelos a considerar ####


# Ajuste manual y visualizacion de residuos
fit1 <- ipc %>%  Arima(order=c(1,1,1), seasonal=c(0,1,1)) 
fit1 %>% residuals() %>% ggtsdisplay()
# Modelo Arima(1,1,1)(0,1,1)


# Ajuste automatico con la funcion auto.arima
fit_Auto <- auto.arima(ipc, seasonal=TRUE)
checkresiduals(fit_Auto)
# Modelo elegido automaticamente: ARIMA(0,1,1)(0,1,1)


# Como podemos ver, en ambos casos los resudios de los modelos parecen seguir una distribución normal,
# sus funciones de autocorrelación simple no muestran retardos significativos y la representación gráfica
# de sus residuos parece tener una varianza y media constantes. Por tanto, los dos modelos pasan el test
# de los residuos, ambos pueden ser validos.


# Coeficientes de los modelos
fit1
fit_Auto


# Accuracy en train
round(accuracy(fit1),3)
round(accuracy(fit_Auto),3)


# Predicciones para la serie completa y comparacion con lo observado
cbind("Indice IPC" = ipc,
      "Valores ajustados" =fitted(fit1)) %>%
    autoplot() + xlab("trimestre") + ylab("") +
    ggtitle("Indice IPC observado y ajustado por el modelo manual")

cbind("Indice IPC" = ipc,
      "Valores ajustados" =fitted(fit_Auto)) %>%
    autoplot() + xlab("trimestre") + ylab("") +
    ggtitle("Indice IPC observado y ajustado por el modelo automatico")



#### Division en Train y Test ####

# Ventanas de ajuste y evaluacion 
ipc_tr<-window(x = ipc, end = c(2018,12))
ipc_tst<-window(x = ipc, start = c(2019,1))



#### Construccion de los modelos ####

# Ajuste manual y visualizacion de residuos
fit1_tr <- Arima(ipc_tr ,order=c(1,1,1), seasonal=c(0,1,1))
fit_auto_tr <- Arima(ipc_tr,order=c(0,1,1), seasonal=c(0,1,1))

# Estudio de residuos
fit1_tr %>% residuals() %>% ggtsdisplay()
checkresiduals(fit1_tr)
fit_auto_tr %>% residuals() %>% ggtsdisplay()
checkresiduals(fit_auto_tr)

# Evaluacion de los modelos en el train set
accuracy(fit1_tr)
accuracy(fit_auto_tr)




#### Generamos las predicciones para el test set con los modelos considerados y evaluamos su performance ####


# Predicciones para el test set
pred1<-forecast(fit1_tr, h=12)
pred_auto<-forecast(fit_auto_tr, h=12)


# Representacion de los valores de test predichos vs observados
pred1 %>% autoplot() + autolayer(ipc_tst, series = 'Real')
pred_auto %>% autoplot() + autolayer(ipc_tst, series = 'Real')


# Evaluacion de los modelos en el test set
accuracy(pred1,ipc_tst)
accuracy(pred_auto,ipc_tst)

# Ambos modelos ofrecen unos resultados casi identicos. El modelo manual parece ofrecer unos resultados mejores
# para el RMSE y el  MAE, no obstante la diferencia es minima. Pese a ser una serie temporal sencilla, los modelos
# estimados ofrecen una performance muy aceptable.



# Representacion conjunta un poco mas visible
autoplot(pred1$mean,series='Pred1') +
    autolayer(pred_auto$mean, series='Pred_auto') +
    autolayer(ipc_tst, series='Real')



#### FINAL ####