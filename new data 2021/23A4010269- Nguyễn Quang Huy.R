# Mo file data
t=file.choose()
# Doc file data
da=read.csv(t,header = T)
head(da)
price=da[,2]
nkg=diff(log(price))
# Goi packages de su dung 
library(fBasics)
library(fGarch)
library(rugarch)
library(tseries)
# Tinh phuong sai, do lech chuan 
basicStats(nkg)
# Ve do thi & test tinh dung
ts.plot(nkg)
adf.test(nkg)
kpss.test(nkg)
# Ve bieu do & test phan phoi chuan
d1=density(nkg)
plot(d1, type='l')
normalTest(nkg, method = c("jb"))
# Kiem dinh tu tuong quan
acf(nkg)
Box.test(nkg,lag=12,type = c("Ljung-Box"))
# Do luong rui ro
# RiskMetrics - ARMA (iGARCH)
spec1=ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="iGARCH",garchOrder=c(1,1)))
m1=ugarchfit(spec = spec1,data=nkg)
m1
ugarchforecast(m1,n,ahead = 10)
v1=0 + qnorm(0.95)*0.03418
VaR1=v1*193000000 
VaR1
setwd("/Users/DELL/OneDrive/Desktop/new data 2021")
source("RMeasure.R")
RMeasure(0,0.03418)
VaR11 = 0.05622110*193000000 
VaR11
# Econometrics Approaches - GARCH
spec2=ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="fGARCH",submodel="GARCH",garchOrder=c(1,1)))
m2=garchFit(~arma(0,0)+garch(1,1),data=nkg,trace = F)
m2
predict(m2,1)
v2=0.001823117 + qnorm(0.95)*0.03213747
VaR2=v2*193000000 
VaR2
RMeasure(0.001823117,0.03213747)
VaR22 = 0.05468455*193000000
VaR22
# Quantile estimation
VaR3=(-quantile(nkg,0.05))*193000000
VaR3
# Monte carlo 
set.seed(123456)
mu=mean(nkg)
sigma=stdev(nkg)
sim=rnorm(1000,mean=mu,sd=sigma)
vaR4=-quantile(sim,0.05)*193000000
vaR4
