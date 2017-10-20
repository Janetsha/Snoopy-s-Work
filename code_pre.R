Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
temp=read.csv("FF_12Ind.csv",header=TRUE)
head(temp)
summary(temp)

## Step 1. Create .RData data file
temp.ts=ts(temp[,-1],start=c(1926,7),freq=12)
myData=timeSeries::as.timeSeries(temp.ts)
head(myData,2);tail(myData,2)
#myData=xts::as.xts(temp.ts)
#head(myData,2);tail(myData,2)
summary(myData)

save(myData,file="FF_12Ind.RData")

##=== Step 2. load .RData and Descriptive Statistics
Input=load("FF_12Ind.RData") 
print(Input)

library(fBasics)
basicStats(myData[,1:3])
basicStats(myData[,1:3])["Sum",]
basicStats(myData[,1:3])[9,]
colStats(myData[,1:3],mean)
colMeans(myData[,1:3])
cov(myData[,1:3])
var(myData[,1:3])
cor(myData[,1:3])

## Step 3. LS Regression
FF_1v=lm(Enrgy~Mkt.RF, data=myData)
summary(FF_1v)
plot(FF_1v)

par(mfrow=c(2,2))
plot(FF_1v)
par(mfrow=c(1,1))

confint(FF_1v, level=0.9)

names(summary(output1))
FF_1v$coef
summary(FF_1v)$coef

AIC(FF_1v)
BIC(FF_1v)
anova(FF_1v)

RawData=as.data.frame(unclass(myData))
FF_pred=predict(FF_1v, interval="confidence")
with(plot(Enrgy~Mkt.RF),data=RawData)
abline(FF_1v,col="blue")
with(lines(FF_pred[,2]~Mkt.RF, lwd=0.1, lty=4, col=2),data=RawData)
with(lines(FF_pred[,3]~Mkt.RF, lwd=0.1, lty=4, col=2),data=RawData)
legend("topleft", c("regression line", "low", "upper"), lty=c(1,4,4), lwd=0.1, bty="n",col=c("blue","red","red"))

FF_3v=lm(Enrgy~Mkt.RF+SMB+HML, data=myData)
summary(FF_3v, corr =TRUE)
anova(FF_1v, FF_3v)
library(car)
linearHypothesis(FF_3v, "Mkt.RF=0")
linearHypothesis(FF_3v, "SMB+HML=0")
linearHypothesis(FF_3v, c("SMB=0","HML=0"))


library(ellipse)
plot(ellipse(FF_3v,c(2,3)),type="l")
points(coef(FF_3v)[2], coef(FF_3v)[3], pch=13)
abline(v=confint(FF_3v)[2,], lty=2)
abline(h=confint(FF_3v)[3,], lty=2)


lm(Enrgy~Mkt.RF+SMB+I(SMB^2), data=myData)
lm(Enrgy~(Mkt.RF+SMB+HML)^2, data=myData)
lm(Enrgy~I(Mkt.RF+SMB+HML)^2, data=myData)

library(sandwich)
coeftest(FF_3v)
coeftest(FF_3v, vcovHC)
coeftest(FF_3v, vcov = vcovHC(FF_3v, type = "HC0"))


library(papeR)
prettify(summary(FF_3v))

table3v=prettify(summary(FF_3v))
write.csv(table3v,file="table3v.csv",row.names=FALSE)