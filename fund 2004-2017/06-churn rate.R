# churn rate
totalbuy <- read_excel("C://Users//shenfan//Desktop//data//2//买入股票总成本.xlsx")
totalbuy <- as.data.table(totalbuy)
setnames(totalbuy, 1:30, c("code", "name", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
totalbuy = melt(totalbuy, id.vars = c("code", "name"), measure.vars = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(totalbuy, "variable", "DateQ")
setnames(totalbuy, 4, c("total.buy"))
totalbuy <- totalbuy[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD]

data <- totalbuy

totalsell <- read_excel("C://Users//shenfan//Desktop//data//2//卖出股票总收入.xlsx")
totalsell <- as.data.table(totalsell)
setnames(totalsell, 1:30, c("code", "name", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
totalsell <- melt(totalsell, id.vars = c("code", "name"), measure.vars = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(totalsell, "variable", "DateQ")
setnames(totalsell, 4, c("total.sell"))
totalsell <- totalsell[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD]

data <- totalsell[data, on = .(DateQ, code)]


#报告期股票市值
stockMV <- read_excel("C://Users//shenfan//Desktop//data//2//股票投资市值.xlsx")
stockMV <- as.data.table(stockMV)
setnames(stockMV, 1:30, c("code", "name", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
stockMV = melt(stockMV, id.vars = c("code", "name"), measure.vars = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(stockMV, "variable", "DateQ")
setnames(stockMV, 4, c("stock.MV"))
stockMV <- stockMV[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD]

data <- stockMV[data, on = .(DateQ, code)]


#调整格式
data <- data[order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, total.sell.2 := shift(total.sell, n = 1, fill = NA, type = "lag"), keyby = code
	][quarter == 4, total.sell := total.sell - total.sell.2
	][, total.buy.2 := shift(total.buy, n = 1, fill = NA, type = "lag"), keyby = code
	][quarter == 4, total.buy := total.buy - total.buy.2
	][, c("total.buy.2", "total.sell.2") := NULL
	][, total.trade := total.buy + total.sell
	][, stock.MV.2 := shift(stock.MV, n = 1, fill = NA, type = "lag"), keyby = code
	][, stock.total.MV := stock.MV + stock.MV.2
	][, turnover.rate := 2 * total.trade / stock.total.MV
	][, churn.rate := min(total.buy, total.sell) / (stock.total.MV / 2), keyby = .(code, DateQ)]


data.churn <- data

#这里可以把NA的都删掉，最后剩下7310数据
#data.churn <- data.churn[total.trade != "NA", .SD
#	][stock.MV.2 != "NA", .SD]

save(data.churn, file = "datachurn.RData")

