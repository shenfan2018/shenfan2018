load("addholdret.RData")
data <- SJ
data <- data[, quarter := quarter(date)]

# 加入funa size
marketvalue <- read_excel("C://Users//shenfan//Desktop//data//2//基金资产净值.xlsx")
marketvalue <- as.data.table(marketvalue)
setnames(marketvalue, 1:30, c("code", "name", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
marketvalue = melt(marketvalue, id.vars = c("code", "name"), measure.vars = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(marketvalue, c("value", "variable"), c("fund_size", "DateQ"))
marketvalue <- marketvalue[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][, logfund_size := log(fund_size)
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][quarter == 2, sem := 1
	][quarter == 4, sem := 2
	][, id := substring(code, 1, 6)
	][, .(id, date, year, sem, logfund_size)]

# 加入
data <- marketvalue[data, on = .(id, year, sem), nomatch = 0]

# add netflow
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


# 加入mkt_ret
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, .(year, month, mkt)]

data <- mkt[data, on = .(year, month)]


# 加入bond_ret
bond <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/10年期国债.xlsx")
bond <- as.data.table(bond)
setnames(bond, 1:2, c("date", "bondyield"))
bond <- bond[, year := year(date)
	][, month := month(date)
	][, .(bondyield = mean(bondyield)), keyby = .(year, month)]

# match
data <- bond[data, on = .(year, month)] 

# 分monthly vol
# 各种因子以及alpha
four.d <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four.d <- as.data.table(four.d)
four.d <- four.d[, date := as.Date(trddy)
	][, year := year(date)
	][, month := month(date)
	][, mkt := mkt_rf + rf
	][, .(vol = sd(mkt)), keyby = .(year, month)
	][year < 2018 & year > 2003
	][, vol.g := ntile(vol, 5)
	][, volhigh := ifelse(vol.g == 5, 1, 0)
	][, .(year, month, volhigh)]

data <- four.d[data, on = .(year, month)]


### 看剩余变量
syf <- data[, .(id, category2, date, year, sem, month, netflow, logfund_size, holding.ret, month_return, TK, PW, LA, CC)]
syf <- syf[order(id, date)]
syf <- syf[, netflow.1 := shift(netflow, n = 3, fill = NA, type = "lead"), keyby = .(id)]

syf[, felm(netflow.1 ~ netflow + logfund_size + TK + month_return  | id + date)] %>% summary()

syf <- syf[, year := year(date)]
syf[, felm(netflow.1 ~ TK | id + date)] %>% summary()

plm(netflow.1 ~ TK, syf, model = "within", effect = "twoways", index = c("id", "date")) %>% summary()


# netflow 6.30的问题
syf <- data[, .(id, category2, date, year, sem, month, netflow, logfund_size, holding.ret, month_return, TK, PW, LA, CC, mkt, bondyield, volhigh)]
zuihou <- syf[, .(id, year, sem, month, TK)
	][month == 6 | month == 12
	][, .(id, year, sem, TK)]
setnames(zuihou, "TK", "z.TK")
#滞后
zuihou <- zuihou[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]

syf <- zuihou[syf, on = .(id, year, sem)]

syf <- syf[, fundgap := month_return - holding.ret
	][order(id, date)
	][, logfund_size.1 := shift(logfund_size, n = 6, fill = NA, type = "lag"), keyby = .(id)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][, netflow.1 := shift(netflow, n = 3, fill = NA, type = "lag"), keyby = .(id)
	][, mkt.1 := shift(mkt, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][, fundgap.1 := shift(fundgap, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][, bondyield.1 := shift(bondyield, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][, volhigh.1 := shift(volhigh, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][order(date, - month_return.1)
	][!is.na(month_return.1)
	][, rank.1 := sequence(.N), by = .(date)
	][, ret_mkt.1 := month_return.1 - mkt.1
#	][, total.r := (.N), keyby = .(date)
#	][, rankp.1 := rank.1 / total.r]
][order(date, - z.TK)
][!is.na(z.TK)
][, TK.rank := sequence(.N), by = .(date)
# ][, total.r := (.N), keyby = .(date)
# ][, TK.rankp := TK.rank / total.r
][, category := ifelse(category2 == "偏股混合型基金", 1, 0)]


syf[, felm(netflow ~ z.TK  + ret_mkt.1 + logfund_size.1 + netflow.1 + fundgap.1  + bondyield.1| year + id)] %>% summary()



syf[, felm(netflow ~ TK.rank + month_return.1 + logfund_size.1 + netflow.1 | id + date)] %>% summary()

syf[, felm(netflow ~ z.TK | id + date)] %>% summary()



############################################################################## quarter
load("addholdret.RData")
data <- SJ
data <- data[, quarter := quarter(date)
	][, .(TK = mean(TK), PW = mean(PW), LA = mean(LA), CC = mean(CC)), keyby = .(id, year, quarter)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# 加入 quarter_return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(quarter_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter)]
# match
data <- ret[data, on = .(id, year, quarter)]


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

#极端值处理
#对netflow
a1 <- quantile(data[, netflow], 0.99, na.rm = TRUE)
a2 <- quantile(data[, netflow], 0.01, na.rm = TRUE)
data <- data[order(netflow)
	][, extremum := ifelse(netflow > a1, 2, ifelse(netflow < a2, 1, 0))
	][extremum == 1, netflow := netflow[.N]
	][extremum == 2, netflow := netflow[1]]


# 加入fund size
marketvalue <- read_excel("C://Users//shenfan//Desktop//data//2//基金资产净值.xlsx")
marketvalue <- as.data.table(marketvalue)
setnames(marketvalue, 1:30, c("code", "name", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
marketvalue = melt(marketvalue, id.vars = c("code", "name"), measure.vars = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(marketvalue, c("value", "variable"), c("fund_size", "DateQ"))
marketvalue <- marketvalue[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][, logfund_size := log(fund_size)
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, id := substring(code, 1, 6)
	][, .(id, year, sem, logfund_size)]

# 加入
data <- marketvalue[data, on = .(id, year, sem), nomatch = 0]

# 分quarterly vol
# 各种因子以及alpha
four.d <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four.d <- as.data.table(four.d)
four.d <- four.d[, date := as.Date(trddy)
	][, year := year(date)
	][, quarter:=quarter(date)
	][, mkt := mkt_rf + rf
	][, .(vol = sd(mkt)), keyby = .(year, quarter)
	][year < 2018 & year > 2003
	][, vol.g := ntile(vol, 5)
	][, volhigh := ifelse(vol.g == 5, 1, 0)
	][, .(year, quarter, volhigh)]

data <- four.d[data, on = .(year, quarter)]


# netflow滞后
data <- data[order(id, year, sem)
	][, netflow.1 := shift(netflow, n = 1, fill = NA, type = "lead"), keyby = id]

reg1 <- data[, felm(netflow.1 ~ TK * volhigh + quarter_return + logfund_size + netflow | id + year)]
reg2 <- data[, felm(netflow.1 ~ PW * volhigh + quarter_return + logfund_size + netflow | id + year)]
reg3 <- data[, felm(netflow.1 ~ LA * volhigh + quarter_return + logfund_size + netflow | id + year)]
reg4 <- data[, felm(netflow.1 ~ CC * volhigh + quarter_return + logfund_size + netflow | id + year)]


# date
date <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)]
data <- date[data, on = .(year, quarter)]


stargazer(reg1, reg2, reg3, reg4, type = "html", out = "C:/Users/shenfan/Desktop/prospect value/tables/netflow and quarter return dummy.doc", add.lines = list(c("Fund", "yes", "yes", "yes", "yes"), c("year", "yes", "yes", "yes", "yes")))
