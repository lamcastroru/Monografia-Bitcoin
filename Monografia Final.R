library(readxl)
library(TSA)
library(FinTS)
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch)
library(tsDyn)
library(ggplot2)
library(forecast)
library(lmtest)
library(aTSA)
library(uroot)
library(rmgarch)
library(rugarch)

c<-read.csv("datos.csv",header=T, sep =";")
attach(c)

Xn = ts(c$BTC,frequency=250)
Yn = ts(c$ORO,frequency=250)

##GRAFICOS DE LAS SERIES

fechas = as.Date(FECHA, "%d/%m/%Y")

np = length(Xn)
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

par(mfrow=c(1,2))
plot(fechas,Xn,xaxt="n", panel.first = grid(), type='l',ylab='valor bitcoin dolar')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

plot(fechas,Yn,xaxt="n", panel.first = grid(), type='l',ylab='valor oro dolares')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

##Descriptivos y grafico de bigotes

basicStats(BTC)
basicStats(ORO)

#Descomposición de la serie

#Bitcoin

m1 = stl(Xn, s.window = 'per', t.window = 50, t.jump = 1)

s1 = m1$time.series[,1]
t1 = m1$time.series[,2]
e1 = m1$time.series[,3]

#oro

p1 = stl(Yn, s.window = 'per', t.window = 50, t.jump = 1)

s2 = p1$time.series[,1]
t2 = p1$time.series[,2]
e2 = p1$time.series[,3]

win.graph()
plot(m1, main="Bitcoin")
plot(p1, main="Oro")

##CALCULAR LOS RENDIMIENTOS
##BITCOIN
rBTC = ts(diff(log(BTC),1,1),frequency=250)

##ORO
rORO = ts(diff(log(ORO),1,1),frequency=250)

#RENDIMIENTOS

win.graph()
plot(fechas[-1],rBTC, xaxt="n", panel.first = grid(),type='l', col="darkgray", ylab="Rendimientos")
lines(fechas[-1],rORO, xaxt="n", panel.first = grid(),type='l', col="red")
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
legend("bottomleft",legend = c("Bitcoin", "Oro") , fill = c("darkgray","red"))


#----------------------acf + banda Bartlett
par(mfrow=c(1,2))
TSA::acf(rBTC, lag.max = 90, ci.type = "ma", drop.lag.0 = TRUE)
TSA::acf(rORO, lag.max = 90, ci.type = "ma", drop.lag.0 = TRUE)

#----------------------prueba Ljung-Box

Box.test(rBTC, lag = 90 , type =  "Ljung-Box")

Box.test(rORO, lag = 90 , type =  "Ljung-Box")

#----------------------prueba Engle efecto arch

#Lagrange Multiplier (LM) test for autoregressive 
#conditional heteroscedasticity (ARCH)

ArchTest(rBTC, lags=1, demean = TRUE)
ArchTest(rORO, lags=1, demean = TRUE)

# Estima la correlacion entre X(n+k) y Y(n) en k

ndiffs(c$ORO)
ndiffs(c$BTC)

win.graph()
ccf(diff(c$BTC),diff(c$ORO))
abline(v=0,col='red')

#-----implementacion de la prueba ADF

aTSA::adf.test(diff(c$BTC)) ##Valor p < 0.05 rechazamos la nula, no hay raiz unitaria
aTSA::adf.test(diff(c$ORO))

##Prueba de causalidad

grangertest(diff(c$BTC),diff(c$ORO),order=1)
grangertest(diff(c$ORO),diff(c$BTC),order=1)

#----------------------estimacion 

##GARCH(1,1)

garch11.1 = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), distribution.model = "norm")
garch.fit11.1= ugarchfit(garch11.1, data = rBTC, fit.control=list(scale=TRUE))
print(garch.fit11.1)
plot(garch.fit11.1, which=3)

forecast1<-ugarchforecast(garch.fit11.1,n.ahead = 3,) 
forecast1
plot(forecast1,which=3)

#-------

garch11.2 = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                       variance.model = list(garchOrder = c(1,1), 
                                             model = "sGARCH"), distribution.model = "norm")
garch.fit11.2= ugarchfit(garch11.2, data = rORO, fit.control=list(scale=TRUE))
print(garch.fit11.2)
plot(garch.fit11.2, which=3)

forecast2<-ugarchforecast(garch.fit11.2,n.ahead = 3,) 
forecast2
plot(forecast2,which=3)

##GARCH(2,1)

garch11.3 = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                       variance.model = list(garchOrder = c(2,1), 
                                             model = "sGARCH"), distribution.model = "norm")
garch.fit11.3= ugarchfit(garch11.3, data = rBTC, fit.control=list(scale=TRUE))
print(garch.fit11.3)
plot(garch.fit11.3, which=3)

r3=garch.fit11.3@fit[["residuals"]]

t = seq(1,length(r3),1)

win.graph()
par(mfrow=c(2,2))
plot(density(r3),xlab='x',main= '')
qqnorm(r3)
qqline(r3,col=2)
acf(r3,60,ci.type="ma",main="")
pacf(r3,60,main="")


Box.test(r3,90)

forecast3<-ugarchforecast(garch.fit11.3,n.ahead = 3,) 
forecast3
plot(forecast3,which=3)

#------

garch11.4 = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                       variance.model = list(garchOrder = c(2,1), 
                                             model = "sGARCH"), distribution.model = "norm")
garch.fit11.4= ugarchfit(garch11.4, data = rORO, fit.control=list(scale=TRUE))
print(garch.fit11.4)
plot(garch.fit11.4, which=3)

forecast4<-ugarchforecast(garch.fit11.4,n.ahead = 3,) 
forecast4
plot(forecast4,which=3)

##GARCH(1,2)

garch11.5 = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                       variance.model = list(garchOrder = c(1,2), 
                                             model = "sGARCH"), distribution.model = "norm")
garch.fit11.5= ugarchfit(garch11.5, data = rBTC, fit.control=list(scale=TRUE))
print(garch.fit11.5)
plot(garch.fit11.5, which=3)

forecast5<-ugarchforecast(garch.fit11.5,n.ahead = 3,) 
forecast5
plot(forecast5,which=3)

#------

garch11.6 = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                       variance.model = list(garchOrder = c(1,2), 
                                             model = "sGARCH"), distribution.model = "norm")
garch.fit11.6= ugarchfit(garch11.6, data = rORO, fit.control=list(scale=TRUE))
print(garch.fit11.6)
plot(garch.fit11.6, which=3)

forecast6<-ugarchforecast(garch.fit11.6,n.ahead = 3,) 
forecast6
plot(forecast6,which=3)

##GARCH(2,2)

garch11.7 = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                       variance.model = list(garchOrder = c(2,2), 
                                             model = "sGARCH"), distribution.model = "norm")
garch.fit11.7= ugarchfit(garch11.7, data = rBTC, fit.control=list(scale=TRUE))
print(garch.fit11.7)
plot(garch.fit11.7, which=3)

forecast7<-ugarchforecast(garch.fit11.7,n.ahead = 3,) 
forecast7
plot(forecast7,which=3)

#------

garch11.8 = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                       variance.model = list(garchOrder = c(2,2), 
                                             model = "sGARCH"), distribution.model = "norm")
garch.fit11.8= ugarchfit(garch11.8, data = rORO, fit.control=list(scale=TRUE))
print(garch.fit11.8)
plot(garch.fit11.8, which=3)

r8=garch.fit11.8@fit[["residuals"]]

t = seq(1,length(r8),1)

par(mfrow=c(2,2))
plot(density(r8),xlab='x',main= '')
qqnorm(r8)
qqline(r8,col=2)
acf(r8,60,ci.type="ma",main="")
pacf(r8,60,main="")

Box.test(r8,90)

forecast8<-ugarchforecast(garch.fit11.8,n.ahead = 3,) 
forecast8
plot(forecast8,which=3)
