require("devtools")
#install_github("topepo/C5.0")

library(mafs)
library(magrittr)
library(forecast)
library(ggplot2)
library(readxl)
library(tseries)
library(tsDyn)
library(readxl)
############################
X20220408060834 <- read_excel("20220408060834.xlsx")
View(X20220408060834)
leite<- ts(X20220408060834[, 2], start = c(1997, 1), frequency = 4)
leite
plot(leite)
# Visualizar decomposi��o sazonal da s�rie
leite%>% decompose %>% plot
# fun��o ggmonthplot do pacote forecast
ggmonthplot(leite)
#O gr�fico da s�rie decomposta mostra que h� fortes componentes de tend�ncia e sazonalidade na s�rie. O componente aleat�rio possui m�dia de 0,13, o que, por ser pr�xima a zero, nos leva a acreditar que a decomposi��o foi bem sucedida. O elemento sazonal da s�rie tamb�m pode ser analisado nos gr�ficos a seguir.
ggseasonplot(leite, year.labels = TRUE) + geom_point() + theme_bw()
#########################
acf(leite,lag.max=12)
pacf(leite,lag.max=12)
acf(diff(leite),lag.max=12)
pacf(diff(leite), lag.max=12)
#Decomposi��o da s�rie temporal em tend�ncia, sazonalidade e resto (componente irregular.  
#Agora usando o comando decompose(), se decomp�e a s�rie em seus componentes tend�ncia, aleat�rio, e sazonal.
z<-decompose(x$Leite.Cru)
plot(z,col='blue',xlab='M�s')
#Estimando o modelo ARIMA com comando:arima(data,order=c(p,d,q)) 
arima(leite,order=c(2,1,1)) 
x<-arima(leite,order=c(2,1,2))
#O termo n.ahead= 4, mostra quatro passos a frente. Ou seja, no caso: novembro de 2020, dezembro de 2020 e janeiro de 2021. 
forecastx<-predict(x,n.ahead=4)
forecastx
checkresiduals(forecastx)
#
ts.plot(window(leite, start=1997,freq=4),forecastx$pred,forecastx$pred+1.96*forecastx$se,forecastx$pred-1.96*forecastx$se,col=c(1,4,2,2), lty=c(1,2,2,2))
#Estimando o modelo ARIMA sazonal 
#arima(leite,order=c(2,1,1),seasonal=list(order=c(2,1,1),period=4))
y<-arima(leite,order=c(2,1,1),seasonal=list(order=c(2,1,1),period=4)) 
forecasty<-predict(y,n.ahead=4) 
forecasty
checkresiduals(forecasty)

ts.plot(window(x$Leite.Cru, start=2010,freq=12),forecasty$pred,forecasty$pred+1.96*forecasty$se,forecasty$pred-(1.96)*forecasty$se,col=c(1,4,2,2), lty=c(1,2,2,2))
#Checagem e diagn�stico. Com o comando tsdiag � poss�vel analisar os gr�ficos dos res�duos (O modelo deve apresentar os res�duos estacion�rios, com m�dia zero e vari�ncia constante).
#lembre-se que o modelo ARIMA(2,1,1) foi chamado de x.
tsdiag(x)
tsdiag(y)
#An�lise da estat�stica de Box?Pierce (e Ljung?Box)
Box.test(x$residuals,lag=1)
Box.test(y$residuals,lag=1)
# Threshold 
grid=selectSETAR(leite,m=1,thDelay=0,trim=0.15,criterion="SSR")
print(grid)
set=setar(leite,m=1,thDelay=0,th=grid$th)
summary(set)

end
