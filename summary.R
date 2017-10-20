setwd("~/Desktop/master/Financial measure/20170929")

#import data#
hs300 <- read.table("2007-201700300.xls", header = T)
head(hs300)
hs300_close <- hs300[ , 3]

#basic summary information
summary(hs300_close)

#眾數＃
as.numeric(names(table(hs300_close)))[which.max(table(hs300_close))] 

#標準差/變異數
sd(hs300_close)
var(hs300_close)

#峰態／偏態
library(moments)
skewness(hs300_close)
kurtosis(hs300_close)
