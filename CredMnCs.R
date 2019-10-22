getwd()
setwd('C:/Users/Admin/Desktop/Projetos/RNI')
getwd()

##pacotes##

library(dplyr)
library(ggplot2)
library(tseries)
library(lmtest)
library(zoo)
library(urca)
library(forecast)
library(FitAR)


##Dados##

cred <- read.csv('CUB2.csv',
                 sep = ";",
                 dec = ",",
                 header = T)

colnames(cred) <- c('data',
                    'cub',
                    'indice',
                    'casa',
                    'casa_deflacionado')


##setar serie de tempo##

tsCasa <- ts(cred$casa,
                frequency = 12,
                start = c(2011,3),
                end = c(2019,8))
tsCasa

tsCasa_Defl <- ts(cred$casa_deflacionado,
                  frequency = 12,
                  start = c(2011,3),
                  end = c(2019,8))

plot(tsCasa)
plot(tsCasa_Defl)


#############teste de estacionariedade########
kpss.test(tsCasa,
          null = c("Level"))


adf.test(tsCasa)
Box.test(tsCasa,lag=20, type='Ljung-Box')

summary(ur.df(tsCasa,
              type='trend', 
              lags = 10, 
              selectlags = "BIC"))


Casa.Diff <- diff(tsCasa,
                     differences = 1)

plot(Casa.Diff)

adf.test(Casa.Diff)


summary(ur.df(Casa.Diff,
              type='trend', 
              lags = 1, 
              selectlags = "BIC"))





##Decomposicao##


CredDecomp <- decompose(tsCasa,
                        type = "additive")

plot(CredDecomp)


##Correlograma##
acf(tsCasa, 
    lag.max = 24)

pacf(tsCasa, 
    lag.max = 24)


##ARIMA##

fitCred <- arima(tsCasa,
                 order=c(2,1,2),
                 seasonal = list(order = c(1,0,0),period = 12),
                 method="ML")

predict(fitCred,
        n.ahead = 24)

fitCredFut <- forecast(fitCred,
                             h=28,
                             level=c(99.5))



plot(fitCredFut)

fitCredFut <- forecast(fitCred,
                       h=10,
                       level=c(99.5))





##ARIMA2 - com CSS##

fitCred_2 <- arima(tsCasa,
                   order=c(2,1,2),
                   seasonal = list(order = c(2,1,2),period = 12),
                   method="CSS")


fitCred_2_Fut <- forecast(fitCred_2,
                       h=28,
                       level=c(90.0))


plot(fitCred_2_Fut,
     main = "Concessões de crédito com recursos direcionados
     - Pessoas físicas - Financiamento imobiliário com taxas reguladas - R$ (milhões)")

par(mfrow = c(1,1))

##teste dos residuos##

acf(fitCred$residuals)

Box.test(fitCred$residuals, lag = 2, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
qqnorm(fitCred$residuals)
qqline(fitCred$residuals)
plot(fitCred$residuals)


