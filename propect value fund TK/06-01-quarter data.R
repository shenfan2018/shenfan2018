load("monthlydata.RData")
data <- fund.TK

# 变成季度
data <- data[, .(TK = mean(TK)), keyby = .(id, year, quarter)]

# ret
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.q <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(quarter_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter)]

data <- data.q[data, on = .(id, year, quarter)]

data <- data[, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# fund size
marketvalue <- read_excel("C://Users//shenfan//Desktop//基金经理//data//2//基金资产净值.xlsx")
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
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, year, sem, logfund_size)]

# match
data <- marketvalue[data, on = .(id, year, sem), nomatch = 0]

# date
date <- read_excel("C:/Users/shenfan/Desktop/prospect value/date.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][month == 3 | month == 6 | month == 9 | month == 12, .SD
	][, month := NULL]

data <- date[data, on = .(year, quarter)]

# fund age
fundage <- read_excel("C://Users//shenfan//Desktop//基金经理//data//股票型混合型//成立日期.xlsx")
fundage <- as.data.table(fundage)
setnames(fundage, 1:2, c("id", "startdate"))
fundage <- data[fundage, on = .(id), nomatch = 0]
fundage <- fundage[, enddate := date]

#计算fund_age
fundage <- fundage[, days := difftime(enddate, startdate, units = c("days"))
	][, fund_age := days / 365
	][, fund_age := as.numeric(fund_age)
	][, logfund_age := log(fund_age)]

add <- fundage[, c("id", "date", "logfund_age")]

data <- add[data, on = .(id, date)]

# inflow outflow netflow
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
	][, end := as.numeric(as.character(end))]

sharechange <- sharechange[order(id, DateQ)
	][, inflow := purchase / beginning
	][, outflow := redemption / beginning
	][, netflow := (purchase - redemption) / beginning
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)]

sharechange <- sharechange[, c("id", "year", "quarter", "inflow", "outflow", "netflow")]

data <- sharechange[data, on = .(id, year, quarter), nomatch = 0]

# add mkt rf
four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, mkt2 := mkt + 1
	][, rf2 := rf + 1
	][, .(quartermkt = prod(mkt2) - 1,quarterrf=prod(rf2)-1), keyby = .(year, quarter)]

data <- mkt[data, on = .(year, quarter)]

# clean wins
data <- data[, colnames(data[, 6:8]) := lapply(.SD[, 6:8], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# 下一期的netflow
data <- data[order(id, date)
	][, netflow.1 := shift(netflow, n = 1, fill = NA, type = "lead"), keyby = .(id)]

syf <- data
syf <- syf[!is.nan(TK)
	][!is.na(netflow.1)]

a <- syf[, yesno := duplicated(logfund_age), keyby = .(id, year, quarter)
	][yesno == FALSE]

write.csv(a, "C://Users//shenfan//Desktop//TKquarterly.csv")

b <- a
b <- b[, mcode := (year - 2009) * 4 + quarter]



plm(netflow.1 ~ TK + logfund_size + logfund_age + quarter_return, a, model = "within", effect = "twoways", index = c("id", "date")) %>% summary()


pmg(netflow.1 ~ TK + logfund_size + logfund_age + quarter_return + netflow, b, model = "mg", index = c("date", "id")) %>% summary()



# fama macbeth的试验
#########################################
pmg(netflow.1 ~ TK, b, model = "mg", index = c("date", "id")) %>% summary() %>% NeweyWest()

pmg(netflow.1 ~ TK, b, model = "mg", index = c("date", "id")) %>% vcovNW() %>% NeweyWest()

syf <- pmg(netflow.1 ~ TK, b, model = "mg", index = c("date", "id"))
NeweyWest(syf, lag = 3)

coeftest(syf, vcov. = NeweyWest)

vcovNW()

pmg(month_return.1 ~ TK + logfund_size + logfund_age + month_return, data, model = "mg", index = c("date", "id")) %>% summary()


