###################### 所有数据应该变成半年度

# stock的东西
load("C:/Users/shenfan/source/repos/shenfan2018/Buffett alpha/stock20030101-20190331.RData")

# mom past 6 month return 
# skewness 
library(moments)
stock.s <- stock
stock.s <- stock.s[order(stock.id, date)
	][, quarter := quarter(date)
	][, year := year(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, Dretwd2 := Dretwd + 1
	][, .(semi_ret = prod(Dretwd2) - 1, skewness = skewness(Dretwd)), keyby = .(stock.id, year, sem)]


max <- stock
max <- max[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][order(stock.id, year, month, - Dretwd)
	][, .SD[1:3], keyby = .(stock.id, year, month)
	][, .(max = mean(Dretwd)), keyby = .(stock.id, year, sem)]

# match
stock.s <- max[stock.s, on = .(stock.id, year, sem)]

# stock TK
load("C:/Users/shenfan/source/repos/shenfan2018/propect value2/PV.RData")
propect <- propect[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][!is.na(TK)
	][, .(stockTK = mean(TK)), keyby = .(stock.id, year, sem)]

# match
stock.s <- propect[stock.s, on = .(stock.id, year, sem)]


# BM
time3 <- c("2003-12-31", "2004-12-31", "2005-12-31", "2006-12-31", "2007-12-31", "2008-12-31", "2009-12-31", "2010-12-31", "2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31", "2018-12-31")
# 账面价值（所有者权益）
BM <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//股东权益合计 年报.xlsx")
BM <- as.data.table(BM)
setnames(BM, 1:18, c("code", "name", time3))
BM <- melt(BM, id.vars = c("code", "name"), measure.vars = time3)
setnames(BM, c("value", "variable"), c("equity", "date"))
BM <- BM[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, equity)
	][order(stock.id, year)]
#流通股市值 12月底
MV <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//月个股流通市值//TRD_Mnth.xlsx")
MV <- as.data.table(MV)
setnames(MV, "Stkcd", "stock.id")
MV <- MV[month == 12
	][, MV := Msmvosd * 1000
	][, .(stock.id, year, MV)]
BM <- MV[BM, on = .(stock.id, year)
	][, BM := equity / MV
	][, .(stock.id, year, BM)]

stock.s <- BM[stock.s, on = .(stock.id, year)]

# GP GP: gross profitable 12月底的 毛利润/lag 总资产
GP <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//毛利润.xlsx")
GP <- as.data.table(GP)
setnames(GP, 1:18, c("code", "name", time3))
GP <- melt(GP, id.vars = c("code", "name"), measure.vars = time3)
setnames(GP, c("value", "variable"), c("mlr", "date"))
AS <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//资产总计.xlsx")
AS <- as.data.table(AS)
setnames(AS, 1:18, c("code", "name", time3))
AS <- melt(AS, id.vars = c("code", "name"), measure.vars = time3)
setnames(AS, c("value", "variable"), c("AS", "date"))
GP <- AS[GP, on = .(code, name, date)]
GP <- GP[!is.na(name)
	][, date := as.Date(as.character(date))
	][order(code, date)
	][, AS.1 := shift(AS, 1, fill = NA, type = 'lag'), keyby = code
	][, GP := mlr / AS.1
	][, stock.id := substring(code, 1, 6)
	][, year := year(date)
	][, .(stock.id, year, GP)]

stock.s <- GP[stock.s, on = .(stock.id, year)]

# size 流通市值
#流通股市值 12月底
MV <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//月个股流通市值//TRD_Mnth.xlsx")
MV <- as.data.table(MV)
setnames(MV, "Stkcd", "stock.id")
MV <- MV[month == 12
	][, MV := Msmvosd * 1000
	][, .(stock.id, year, MV)]

stock.s <- MV[stock.s, on = .(stock.id, year)]


## 删除2018 2019
stock.s <- stock.s[year < 2018]

# 选择A股
A <- read_excel("C:/Users/shenfan/Desktop/Buffett alpha/A股.xlsx")
A <- as.data.table(A)
setnames(A, 1:2, c("code", "name"))
A <- A[, stock.id := substring(code, 1, 6)
	][, .(stock.id, name)]

stock.s <- A[stock.s, on = .(stock.id), nomatch = 0]

save(stock.s, file = "stocks.RData")

