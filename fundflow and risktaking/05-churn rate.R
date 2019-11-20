# churn rate
totalbuy1 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//股票型 买入股票总成本.xlsx")
totalbuy2 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//混合型 买入股票总成本.xlsx")
totalbuy <- rbindlist(list(totalbuy1, totalbuy2))
totalbuy <- as.data.table(totalbuy)
setnames(totalbuy, 1:34, c("code", "name", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
totalbuy= melt(totalbuy, id.vars = c("code", "name"), measure.vars = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
setnames(totalbuy, "variable", "DateQ")
setnames(totalbuy, 4, c("total.buy"))
totalbuy <- totalbuy[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD
	][, id := substring(code, 1, 6)]

data <- totalbuy

totalsell1 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//股票型 卖出股票总成本.xlsx")
totalsell2 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//混合型 卖出股票总成本.xlsx")
totalsell <- rbindlist(list(totalsell1, totalsell2))
totalsell <- as.data.table(totalsell)
setnames(totalsell, 1:34, c("code", "name", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
totalsell <- melt(totalsell, id.vars = c("code", "name"), measure.vars = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
setnames(totalsell, "variable", "DateQ")
setnames(totalsell, 4, c("total.sell"))
totalsell <- totalsell[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD
	][, id := substring(code, 1, 6)]

data <- totalsell[data, on = .(DateQ,code,id)]


#报告期股票市值
stockMV1 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//股票型 股票投资市值.xlsx")
stockMV2<- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//混合型 股票投资市值.xlsx")
stockMV <- rbindlist(list(stockMV1, stockMV2))
stockMV <- as.data.table(stockMV)
setnames(stockMV, 1:34, c("code", "name", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
stockMV = melt(stockMV, id.vars = c("code", "name"), measure.vars = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
setnames(stockMV, "variable", "DateQ")
setnames(stockMV, 4, c("stock.MV"))
stockMV <- stockMV[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD
	][, id := substring(code, 1, 6)]

data <- stockMV[data, on = .(DateQ, code, id)]


#调整格式
data <- data[order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, total.sell.2 := shift(total.sell, n = 2, fill = NA, type = "lag"), keyby = code
	][quarter == 4, total.sell := total.sell - total.sell.2
	][, total.buy.2 := shift(total.buy, n = 2, fill = NA, type = "lag"), keyby = code
	][quarter == 4, total.buy := total.buy - total.buy.2
	][, c("total.buy.2", "total.sell.2") := NULL
	][, total.trade := total.buy + total.sell
	][, stock.MV.2 := shift(stock.MV, n = 2, fill = NA, type = "lag"), keyby = code
	][quarter == 4 | quarter == 2, stock.total.MV := stock.MV + stock.MV.2
	][, churn.rate := 2 * total.trade / stock.total.MV]

data.churn <- data

#这里可以把NA的都删掉，最后剩下7310数据
#data.churn <- data.churn[total.trade != "NA", .SD
#	][stock.MV.2 != "NA", .SD]

save(data.churn, file = "datachurn.RData")
