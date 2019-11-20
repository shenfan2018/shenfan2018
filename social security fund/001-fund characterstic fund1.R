# 改fund 2004-2017

#添加市值
marketvalue <- read_excel("C://Users//shenfan//Desktop//基金经理//data//2//基金资产净值.xlsx")
marketvalue <- as.data.table(marketvalue)
setnames(marketvalue, 1:30, c("code", "name", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
marketvalue = melt(marketvalue, id.vars = c("code", "name"), measure.vars = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(marketvalue, c("value", "variable"), c("fund_size", "DateQ"))
marketvalue <- marketvalue[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][, logfund_size := as.data.table(log(fund_size))
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][quarter == 2, sem := 1
	][quarter == 4, sem := 2
	][!code == "数据来源：Wind"
	][!is.na(code)
	][, id := substring(code, 1, 6)]

# 加入
data <- marketvalue

#加入csmar的inflow outflow
sharechange <- read_excel("C://Users//shenfan//Desktop//基金经理//data//2//份额变动文件//Fund_ShareChange.xlsx")
sharechange <- as.data.table(sharechange)
setnames(sharechange, 2:11, c("class", "id", "type", "start", "DateQ", "beginning", "purchase", "redemption", "split", "end"))
#调格式,若用5和6的话，beginning有问题
sharechange <- sharechange[id != "基金代码'", .SD
	][id != "没有单位'"
	][, type := as.numeric(as.character(type))
	][type == 1 | type == 2 | type == 3 | type == 4, .SD
	][, DateQ := as.Date(as.character(DateQ))
	][, beginning := as.numeric(as.character(beginning))
	][, purchase := as.numeric(as.character(purchase))
	][, redemption := as.numeric(as.character(redemption))
	][, end := as.numeric(as.character(end))
	][type == 1 | type == 2, sem := 1
	][type == 3 | type == 4, sem := 2
	][, year := year(DateQ)]

#计算半年的
sharechange <- sharechange[order(id, DateQ)
	][, purchase2 := sum(purchase, na.rm = FALSE), keyby = .(id, year, sem)
	][, redemption2 := sum(redemption, na.rm = FALSE), keyby = .(id, year, sem)
	][type == 1 | type == 3, inflow := purchase2 / beginning
	][type == 1 | type == 3, outflow := redemption2 / beginning
	][type == 1 | type == 3, netflow := (purchase2 - redemption2) / beginning
	][type == 1 | type == 3, .SD]

sharechange <- sharechange[, c("id", "year", "sem", "inflow", "outflow", "netflow")]

data <- sharechange[data, on = .(id, year, sem)]

#fund style(wind+csmar)
fund.csmar <- read_excel("C://Users//shenfan//Desktop//基金经理//data//基金类型风格//Fund_MainInfo.xlsx")
fund.csmar <- as.data.table(fund.csmar)
setnames(fund.csmar, 1:2, c("id", "name"))
fund.csmar <- fund.csmar[name != "基金全称'", .SD
	][name != "没有单位'", .SD
	][, name := NULL]

fund.wind <- read_excel("C://Users//shenfan//Desktop//基金经理//data//基金类型风格//windstyle.xlsx")
fund.wind <- as.data.table(fund.wind)
setnames(fund.wind, 1:6, c("code", "name", "category2", "category1", "style1", "style2"))
fund.wind <- fund.wind[, id := substring(code, 1, 6)
	][, c("style1", "style2", "name", "category1") := NULL]
fund.category <- fund.csmar[fund.wind, on = .(id)]
#match
data <- fund.category[data, on = .(id), nomatch = 0]

#选择混合型和股票型
data <- data[Category == "股票型基金" | Category == "混合型基金", .SD
	][category2 == "偏股混合型基金" | category2 == "普通股票型基金", .SD]

#添加fund_age
fundage <- read_excel("C://Users//shenfan//Desktop//基金经理//data//股票型混合型//成立日期.xlsx")
fundage <- as.data.table(fundage)
setnames(fundage, 1:2, c("id", "startdate"))
fundage <- data[fundage, on = .(id), nomatch = 0]
fundage <- fundage[, enddate := DateQ]
#计算fund_age
fundage <- fundage[, days := difftime(enddate, startdate, units = c("days"))][
	, fund_age := days / 365][
	, fund_age := as.numeric(fund_age)][
	  , logfund_age := log(fund_age)]
add <- fundage[, c("code", "DateQ", "fund_age", "logfund_age")]
data <- add[data, on = .(code, DateQ), nomatch = 0]

fund.1 <- data[!is.nan(logfund_age)
	][, .(code, id, Category, InvestmentStyle, category2, DateQ, year, quarter, sem, fund_age, logfund_age, fund_size, logfund_size, inflow, outflow, netflow)]

save(fund.1, file = "fund1.RData")