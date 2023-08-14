library(quantmod)
library(MLmetrics)
library(tidyverse)
library(forecast)
library(astsa)
library(TSA)
library(FinTS)
library(tseries)
library(psych)

first_date <- '2015-02-01' 
last_date <- '2020-06-01' 
my_ticker <- '^IBEX' 
series_name <- 'IBEX'
first_date_training = '2003-02-01' 
last_date_training = '2020-01-31'
first_date_test <- '2020-02-01' 
last_date_test <- '2020-11-01'
getSymbols(my_ticker, from=first_date, to=last_date)
IBEX %>% na.omit()
par(mfrow = c(1,2))
acf(log_ret)
pacf(log_ret)
eacf(log_ret)
eacf(log_ret)
log_ret = dailyReturn(IBEX$IBEX.Adjusted,type = "log")
p1 = ggplot(datos, aes(x = ref.date, y = price.adjusted)) + 
  geom_line() + labs(title = "Serie Precios IBEX",caption = "Fuente: Elaboración propia con R") + labs(x = "Tiempo",y = "Precio")

p2 = ggplot(datos, aes(x = ref.date, y = log_ret)) + 
  geom_line() + labs(title = "Serie Rendimientos IBEX",caption = "Fuente: Elaboración propia con R") + labs(x = "Tiempo",y = "Rendimiento") 

describe(IBEX$IBEX.Adjusted)
dates = as.POSIXct(log_ret)
length(log_ret)

plot(IBEX$IBEX.Adjusted,main = "Precios")
best_arima = auto.arima(log_ret,trace = T,test = "adf",ic = 'aic')
adf.test(log_ret, alternative = "stationary")
ma_2 = arima(log_ret,order = c(0,0,2), method = "ML")
ar_2 = arima(log_ret,order = c(2,0,0), method = "ML")
ar_2_sarima= sarima(log_ret,2,0,0)
residuos_AR2 = xts(ar_2$residuals,order.by = dates)
residuos_AR2_2= residuos_AR2^2
p3 =ggplot(data= residuos_AR2,aes(x =dates,y = residuos_AR2)) + geom_line()+ labs(title = "Residuos modelo AR2",caption = "Fuente: Elaboración propia con R") + labs(x = "Tiempo",y = "Residuos")
p4 = ggplot(data= residuos_AR2_2,aes(x =dates,y = residuos_AR2_2)) + geom_line()+ labs(title = "Residuos modelo AR2 al cuadrado",caption = "Fuente: Elaboración propia con R") + labs(x = "Tiempo",y = "Residuos al cuadrado")
x= seq(-0.2,0.2,length = 100)
hx = dt(x,df = 10)
plot(x,hx,type = "l",col = "red")
p5 = 
sarima(log_ret,2,0,0)
sarima(log_ret,2,0,0,xreg = covid_xts)
ArchTest(ma_2$residuals, lags=5, demean=TRUE)
ArchTest(ma_2$residuals, lags=1, demean=TRUE)
ArchTest(ma_2$residuals, lags=10, demean=TRUE)
Box.test(ma_2$residuals, type = "Ljung-Box", lag = 3, fitdf = 2)
Box.test(ma_2$residuals, type = "Ljung-Box", lag = 5, fitdf = 2)
Box.test(ma_2$residuals, type = "Ljung-Box", lag = 7, fitdf = 2)
Box.test(ma_2$residuals, type = "Ljung-Box", lag = 18, fitdf = 2)
ArchTest(ar_2$residuals, lags=2, demean=TRUE)
ArchTest(ar_2$residuals, lags=5, demean=TRUE)
ArchTest(ar_2$residuals, lags=10, demean=TRUE)

Box.test(ar_2$residuals, type = "Ljung-Box", lag = 3, fitdf = 2)
Box.test(ar_2$residuals, type = "Ljung-Box", lag = 5, fitdf = 2)
Box.test(ar_2$residuals, type = "Ljung-Box", lag = 7, fitdf = 2)
Box.test(ar_2$residuals, type = "Ljung-Box", lag = 18, fitdf = 2)
Box.test(log_ret,type = "Ljung-Box",lag = 2)
sarima1 = sarima(log_ret,2,0,0)

acf(log_ret^2)
acf(ar_2$residuals)
acf(ar_2$residuals^2)

pacf(log_ret^2)

#Decrece acf exponencial y pacf tiene un comportamiento sinusoidal de PACF desde el 5 retardo, asique parece ser garch(1,1)
par(mfrow = c(1,2))
acf(ar_2$residuals)
acf(ar_2$residuals^2)

pacf(ar_2$residuals^2)

hist(ar_2$residuals,breaks = 100)
plot(ar_2$residuals)
plot(ar_2$residuals^2)
#Variable covid
library(xts)
covid = ifelse(dates> "2020-02-25"&dates<'2020-06-01',1,0)
covid_xts = xts(covid,order.by = dates)

eacf(log_ret^2)
eacf(ma_2$residuals^2)
eacf(ar_2$residuals^2)
GARCH_20 = ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(2,0)),
                      mean.model = list(armaOrder = c(2,0)), 
                      distribution.model = "sstd")
GARCH_11 = ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(2,0)), 
                      distribution.model = "sstd")
GARCH_12 = ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(1,2)),
                      mean.model = list(armaOrder = c(2,0)), 
                      distribution.model = "sstd")
GARCH_21 = ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(2,1)),
                      mean.model = list(armaOrder = c(2,0)), 
                      distribution.model = "sstd")
GARCH_22 = ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(2,2)),
                      mean.model = list(armaOrder = c(2,0)), 
                      distribution.model = "sstd")

GARCH_11N = ugarchspec(variance.model = list(model = "sGARCH",
                                             garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(0,0)), 
                       distribution.model = "sstd")
fit_garch = ugarchfit(spec= GARCH_11N,data = log_ret)
fit_20 = ugarchfit(spec= GARCH_20,data = log_ret)
fit_11 = ugarchfit(spec= GARCH_11,data = log_ret)
fit_12 = ugarchfit(spec= GARCH_12,data = log_ret)
fit_21 = ugarchfit(spec= GARCH_21,data = log_ret)
fit_22 = ugarchfit(spec= GARCH_22,data = log_ret)
stand_resid = fit_11@fit$residuals/fit_11@fit$sigma

stand_resid = xts(stand_resid,order.by =dates )

ArchTest(stand_resid,lags =10,demean = T)

plot(stand_resid)
Garch_fr = ugarchforecast(fit_11,n.ahead = 100)
Garch_fr_5 = ugarchforecast(fit_11,n.ahead = 5)


library(fitdistrplus)
fitdistr(distr =  'sstd' ,data = log_ret)$pars
fitdist(distr =  'sstd' , x = log_ret,method = "mle")$pars
hist(ar_2$residuals,breaks = 100)
skewness(ar_2$residuals)
kurtosis(ar_2$residuals) 
cat( "Para un nivel de significación alpha = 0.05, el valor de cuantil de la distribución t-student para los retornos es: " , 
     qdist(distribution = 'sstd' , shape = 5.48,skew = 0.89  , p = 0.01) , sep = "")
VaRplot =   geom_line(aes(y = Garch_fr_5@forecast$seriesFor + Garch_fr_5@forecast$sigmaFor*(-1.65)), colour = 'red') +
  labs(x = '' , y = 'Retornos' , title = 'Value at Risk out-sample')
Var_est = Garch_fr@forecast$seriesFor + Garch_fr@forecast$sigmaFor*(-1.65)
plot(Var_est,type ="l")
plot(fit_11)
GARCH = ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(0,0)), 
                   distribution.model = "sstd")
fit_GARCH = ugarchfit(GARCH,data = log_ret)

bootp_GARCH = ugarchboot(fit_GARCH, method = c("Partial", "Full")[1],n.ahead = 109, n.bootpred = 500)

bootp = ugarchboot(fit_11, method = c("Partial", "Full")[1],n.ahead = 109, n.bootpred = 500)

plot(bootp)



VaR = bootp@forc@forecast$seriesFor + bootp@forc@forecast$sigmaFor* (-1.65)
VaR2 = bootp@forc@forecast$seriesFor + bootp@forc@forecast$sigmaFor* (-2.78)
#Obtenemos los 100 proximos retornos para evaluar la capacidad de prediccion del modelo
first_date_test <- '2020-06-02' 
last_date_test <- '2020-11-01'
getSymbols(my_ticker, from=first_date_test, to=last_date_test)
IBEX %>% na.omit()

log_ret_test = dailyReturn(IBEX$IBEX.Adjusted,type = "log")

dates = as.POSIXct(log_ret_test)



VaR = xts(VaR,order.by = dates)
VaR2 = xts(VaR2,order.by = dates)

plot(VaR)
#Elegimos los rendimientos reales

VaRplot3= qplot(y = log_ret_test , x =dates , geom = 'point') + geom_point(colour = 'black' , size = 2) + 
  geom_line(aes(y = bootp@forc@forecast$seriesFor + bootp@forc@forecast$sigmaFor* (-1.65)
, x = dates) , colour = 'red') +geom_line(aes(y = bootp@forc@forecast$seriesFor + bootp@forc@forecast$sigmaFor* (-2.78)
                                              , x = dates) , colour = 'blue')+
  labs(x = '' , y = 'Retornos' , title = 'Value at Risk out-sample',caption = "Fuente: Elaboración propia con R")
+

VaRplot2= qplot(y = log_ret_test , x =dates , geom = 'point') + geom_point(colour = 'black' , size = 2) + 
  geom_line(aes(y = bootp@forc@forecast$seriesFor + bootp@forc@forecast$sigmaFor* (-2.78)
                , x = dates) , colour = 'red') +
  labs(x = '' , y = 'Retornos' , title = 'Value at Risk out-sample')
library(segMGarch)
kupiec(log_ret_test, VaR = VaR, VaR_level = 0.05, verbose = TRUE, test = "PoF")


menores_VaR_GARCH = numeric(length = 109)
for (i in 1:109){
  if (log_ret_test[i] >= VaR2[i]){
    menores_VaR_GARCH[i] = 0
  }
  else{
    menores_VaR_GARCH[i]= 1
  }
}
sum(menores_VaR_GARCH)
qt(0.01, 108, lower.tail = F)
qt(0.05, 108, lower.tail = F)



#Presentacion

