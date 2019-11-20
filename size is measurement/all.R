# return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
data.NAV <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

time = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31")
#添加市值
marketvalue <- read_excel("C://Users//shenfan//Desktop//基金经理/data//2//基金资产净值.xlsx")
marketvalue <- as.data.table(marketvalue)
setnames(marketvalue, 1:30, c("code", "name", time))
marketvalue = melt(marketvalue, id.vars = c("code", "name"), measure.vars = time)
setnames(marketvalue, c("value", "variable"), c("fund_size", "DateQ"))
marketvalue <- marketvalue[, DateQ := as.Date(DateQ)
	][, logfund_size := as.data.table(log(fund_size))
	][, id := substring(code, 1, 6)
	][order(id, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(id, year, sem, logfund_size)]

# match
data <- marketvalue[data.NAV, on = .(id, year, sem)]

# style
#fund style(wind+csmar)
fund.wind <- read_excel("C://Users//shenfan//Desktop//基金经理//data//基金类型风格//windstyle.xlsx")
fund.wind <- as.data.table(fund.wind)
setnames(fund.wind, 1:6, c("code", "name", "category2", "category1", "style1", "style2"))
fund.wind <- fund.wind[, id := substring(code, 1, 6)
	][, c("style1", "style2", "name", "category1") := NULL
	][category2 == "偏股混合型基金" | category2 == "普通股票型基金", .SD
	][, .(id, category2)]

#match
data <- fund.wind[data, on = .(id), nomatch = 0]

# delete fund_size NA
data <- data[!is.na(logfund_size)]

# clean
# month_return
a1 <- quantile(data[, month_return], 0.999, na.rm = TRUE)
a2 <- quantile(data[, month_return], 0.001, na.rm = TRUE)
data <- data[order(month_return)
	][, extremum := ifelse(month_return > a1, 2, ifelse(month_return < a2, 1, 0))
	][extremum == 1, month_return := month_return[.N]
	][extremum == 2, month_return := month_return[1]]

# logfund_size
a1 <- quantile(data[, logfund_size], 0.999, na.rm = TRUE)
a2 <- quantile(data[, logfund_size], 0.001, na.rm = TRUE)
data <- data[order(logfund_size)
	][, extremum := ifelse(logfund_size > a1, 2, ifelse(logfund_size < a2, 1, 0))
	][extremum == 1, logfund_size := logfund_size[.N]
	][extremum == 2, logfund_size := logfund_size[1]]

# fund size sort
sort <- data
sort <- sort[order(id, year, month)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, size.g := ntile(logfund_size, 5), keyby = .(year, month)
	][, .(ret = mean(month_return.1, na.rm = TRUE)), keyby = .(year, month, size.g)
	][!is.na(ret)]

low <- sort[size.g == 1]
high <- sort[size.g == 5]
dif <- low[high, on = .(year, month)
	][, dif := i.ret - ret]

t.test(sort[size.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

# 6 factor
sort <- sort[, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

# match
sort <- four[sort, on = .(year, month)
	][, ret_rf := ret - rf]

reg5 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, sort[size.g == 5])
reg4 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, sort[size.g == 4])
reg3 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, sort[size.g == 5])
reg2 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, sort[size.g == 2])
reg1 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, sort[size.g == 1])

low <- sort[size.g == 1]
high <- sort[size.g == 5]
dif <- low[high, on = .(year, month)
	][, dif := i.ret - ret]
dif <- four[dif, on = .(year, month)]

regd <- lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, dif)

stargazer(reg1, reg2, reg3, reg4, reg5, regd, type = "html", out = "C:/Users/shenfan/Desktop/factor.doc")



