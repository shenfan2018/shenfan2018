# skewness
# 所有stock的monthly return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/stock.RData")
library(moments)
#整理
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)]

skewness <- stock
skewness <- skewness[order(stock.id, date)
	][, .(skewness = skewness(Dretwd)), keyby = .(stock.id, year, month)]

save(skewness, file = "skewness.RData")

# max
max <- stock
max <- max[order(stock.id, year, month, - Dretwd)
	][, .SD[1:3], keyby = .(stock.id, year, month)
	][, .(max = mean(Dretwd)), keyby = .(stock.id, year, month)]

save(max, file = "max.RData")

