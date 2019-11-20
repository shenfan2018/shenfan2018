# TK和month_return
fund.TK <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/fund-returnwithTK20190328.xlsx")
fund.TK <- as.data.table(fund.TK)
# 所有char变成numeric
fund.TK <- fund.TK[, colnames(fund.TK[, 7:18]) := lapply(.SD[, 7:18], as.numeric)
	][, date := as.Date(date)]

# id的问题
fund.TK <- fund.TK[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")]

fund.TK <- fund.TK[, - (11:18)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# quarter return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.NAV <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)]

ret <- data.NAV

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)
	][, month_return2 := month_return + 1]

reg.roll <- list()
for (i in 3:168) {
	reg.roll[[i]] <- ret[tag >= i - 2 & tag <= i, {
		I <- prod(month_return2) - 1
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]

reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

ret <- reg.cof[ret, on = .(tag, id)]

setnames(ret, 3, "quarter_return")

# 已经是下一期的季度了
ret <- ret[, quarter_return.1 := shift(quarter_return, n = 3, fill = NA, type = "lead"), keyby = .(id)]

q.ret <- ret[, .(id, year, month, quarter_return.1)]

# match
fund.TK <- q.ret[fund.TK, on = .(id, year, month)]

# fund size
marketvalue <- read_excel("C://Users//shenfan//Desktop//data//2//基金资产净值.xlsx")
marketvalue <- as.data.table(marketvalue)
setnames(marketvalue, 1:30, c("code", "name", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
marketvalue = melt(marketvalue, id.vars = c("code", "name"), measure.vars = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(marketvalue, c("value", "variable"), c("fund_size", "DateQ"))
marketvalue <- marketvalue[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][, logfund_size := as.data.table(log(fund_size))
	][, id := substring(code, 1, 6)
	][order(id, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][quarter == 2, sem := 1
	][quarter == 4, sem := 2
	][, .(id, year, sem, logfund_size)]

# match
fund.TK <- marketvalue[fund.TK, on = .(id, year, sem)]


# fund age
fundage <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//成立日期.xlsx")
fundage <- as.data.table(fundage)
setnames(fundage, 1:2, c("id", "startdate"))
fundage <- fund.TK[fundage, on =.(id), nomatch = 0]
fundage <- fundage[, enddate := date]

#计算fund_age
fundage <- fundage[, days := difftime(enddate, startdate, units = c("days"))
	][, fund_age := days / 365
	][, fund_age := as.numeric(fund_age)
	][, logfund_age := log(fund_age)]

add <- fundage[, c("id", "date", "logfund_age")]

fund.TK <- add[fund.TK, on = .(id, date)]


# churn rate
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/datachurn.RData")
data.churn <- data.churn[, id := substring(code, 1, 6)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, year, sem, turnover.rate, churn.rate)]

# match
fund.TK <- data.churn[fund.TK, on = .(id, year, sem)]


# netflow
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

# match
fund.TK <- fundflow[fund.TK, on = .(id, year, quarter)]


# holding 的TK
load("C:/Users/shenfan/source/repos/shenfan2018/propect value2/fundPV-st1.RData")
setnames(fund.PV, 5:8, c("p.TK", "p.PW", "p.LA", "p.CC"))
fund.PV <- fund.PV[, date := NULL]

fund.TK <- fund.PV[fund.TK, on = .(id, year, month)]

save(fund.TK, file = "monthlydata.RData")
