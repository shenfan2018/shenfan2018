load("fundPV.RData")
data <- fund.PV

# 加入monthly return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, sem, month)]

data <- ret[fund.PV, on = .(id, year, month)
	][, quarter := quarter(date)]

# 加入net flow
time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")
# read
fundflow1 <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/股票型 单季度净申购赎回率.xlsx")
fundflow2 <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/混合型 单季度净申购赎回率.xlsx")
fundflow <- rbindlist(list(fundflow1, fundflow2))
fundflow <- as.data.table(fundflow)
setnames(fundflow, 1:58, c("code", "name", time))
fundflow = melt(fundflow, id.vars = c("code", "name"), measure.vars = time)
setnames(fundflow, "variable", "DateQ")
setnames(fundflow, 4, c("netflow"))
fundflow <- fundflow[, DateQ := as.Date(DateQ)
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD
	][, id := substring(code, 1, 6)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, .(id, year, quarter, netflow)]

data <- fundflow[data, on = .(id, year, quarter)]

## clean
#极端值处理
#对netflow
a1 <- quantile(data[, netflow], 0.99, na.rm = TRUE)
a2 <- quantile(data[, netflow], 0.01, na.rm = TRUE)
data <- data[order(netflow)
	][, extremum := ifelse(netflow > a1, 2, ifelse(netflow < a2, 1, 0))
	][extremum == 1, netflow := netflow[.N]
	][extremum == 2, netflow := netflow[1]]

# category
fund.wind <- read_excel("C://Users//shenfan//Desktop//data//基金类型风格//windstyle.xlsx")
fund.wind <- as.data.table(fund.wind)
setnames(fund.wind, 1:6, c("code", "name", "category2", "category1", "style1", "style2"))
fund.wind <- fund.wind[, id := substring(code, 1, 6)
	][, .(id, category2)]
#match
data <- fund.wind[data, on = .(id), nomatch = 0]

data <- data[, .(id, date, category2, netflow, month_return, TK, PW, LA, CC)
	][order(id, date)]

clean <- data

save(clean, file = "3clean.RData")
