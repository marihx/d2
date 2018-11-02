#Avaliação D2

## Moeda utilizada: Bancor - período de abril/2017 a outubro/2018

install.packages("strucchange")
install.packages("forecast")
install.packages("readxl")
install.packages("aTSA")
install.packages("urca")
install.packages("tseries")
install.packages("pwt8")
remove.packages("readxl")
install.packages("readxl", dependencies = T)
install.packages("strucchange")
remove.packages("aTSA")
install.packages("aTSA", dependencies = T)

library(strucchange)
library(forecast)
library(readxl)
library(aTSA)
library(urca)
library(tseries)
library(pwt8)

Bancor <- na.omit(read_excel("C:/Econometria/Bancor.xls"))

library(readxl)
Bancor <- read_excel("C:/Econometria/Bancor.xls", 
                     col_types = c("date", "numeric"))
View(Bancor)

BANCOR <- ts <- ts(Bancor$Close, start = 2017, frequency = 365)

plot(BANCOR)



##Função de Autocorrelação
###Verificar se a Série é Estacionária
####Criar FAC  e FACP

acf(Bancor$Close,lend=2, lwd=3,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(Bancor$Close,lend=60, lwd=3,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

#Teste ADF
ur.df(BANCOR, "none", lags = 1)

#Teste Philips-Perron
pp.test(BANCOR)

#Teste KPSS
kpss.test(BANCOR)

#Se não for estacionária, diferenciar a série

IntOrdem1 <- diff(log(Bancor$Close))
IntegradaOrdem1 <- ts(IntOrdem1, start = 2017, frequency = 365)

plot(IntegradaOrdem1, type="l", main="Primeira Diferança dos Logs do Bancor - LogReturn", ylab="Log Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")

#A série é estacionária, porém segue partes para Verificar se a Série se tornou Estacionária

#FAC e FACP

acf(IntOrdem1,lend=2, lwd=3,col="darkred",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(IntOrdem1,lend=60, lwd=3,col="darkred",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

#Teste ADF
ur.df(IntegradaOrdem1, "none", lags = 1)

#Teste Philips-Perron
pp.test(IntegradaOrdem1)

#Teste KPSS
kpss.test(IntegradaOrdem1)


#Teste Bai Perron

bp_ts <- breakpoints(BANCOR ~ 1)
bp_ts
summary(bp_ts)
plot(BANCOR, type="l")               
lines(bp_ts) 



MIO1 <- diff(BANCOR)
plot(MIO1)

##MODELO ARIMA 
#MA(0)
#AR(0)
#I(1)

AR1 <- arima(BANCOR, order = c(0,1,0))   #Estima o AR1 e salva os resultados como AR1
MA1 <- arima(BANCOR, order = c(0,1,0))

arima010 <- arima(BANCOR, c(0,1,0))

estimacoes <- list(arima010)

AIC <- sapply(estimacoes, AIC)
AIC <- as.data.frame(AIC)
BIC <- sapply(estimacoes, BIC)
BIC <- as.data.frame(BIC)
AIC
BIC

Modelo <-c("arima010") 
Resultados <- data.frame(Modelo,AIC,BIC)
View(Resultados)

melhor_modelo <- arima010 

previsto <- predict(melhor_modelo,15)      #Previsão para os próximos 1 anos seguindo o melhor modelo estimado
View(previsto$pred)                       #Valores previstos
previsto1 <- forecast(melhor_modelo,15)    #Mesma previsão mas pelo comando forecast que retorna um intervalo de confiança
previsto1
plot(previsto1)

predict(arima010,15)
predict(melhor_modelo,15)

library(forecast)
forecast(melhor_modelo,15)
forecast(arima010,15)



plot(forecast(arima010,15))
grid(col = "black", lty = "dotted")

plot(forecast(melhor_modelo,15))
grid(col = "black", lty = "dotted")

arch.test(melhor_modelo)

melhor_modelo


