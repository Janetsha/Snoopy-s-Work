setwd("~/Desktop/master/Financial measure/20171020")

df <- xlsx::read.xlsx("A103230008.xlsx",header = T, sheetIndex = 3)
hist(df[ ,2])
