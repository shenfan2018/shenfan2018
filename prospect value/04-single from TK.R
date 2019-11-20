# 042 single sorting from TK
# 加入flow
time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")

#以fundflow做一个主表data.main
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

load("monthlyTK.RData")
SJ <- as.data.table(SJ)
SJ <- SJ[, quarter := quarter(date)]
SJ <- fundflow[SJ, on = .(id, year, quarter)]

## clean
#极端值处理
#对netflow
a1 <- quantile(SJ[, netflow], 0.99, na.rm = TRUE)
a2 <- quantile(SJ[, netflow], 0.01, na.rm = TRUE)
SJ <- SJ[order(netflow)
	][, extremum := ifelse(netflow > a1, 2, ifelse(netflow < a2, 1, 0))
	][extremum == 1, netflow := netflow[.N]
	][extremum == 2, netflow := netflow[1]]

#fund flow的一些滞后
SJ <- SJ[order(id, year, month)
	][, netflow.1 := shift(netflow, n = 3, fill = NA, type = "lag"), keyby = id
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lag"), keyby = id]

# 一些差分
SJ <- SJ[order(id, year, month)
	][, ztk.c := c(NA, diff(ztk, difference = 1)), keyby = id]

# category
fund.wind <- read_excel("C://Users//shenfan//Desktop//data//基金类型风格//windstyle.xlsx")
fund.wind <- as.data.table(fund.wind)
setnames(fund.wind, 1:6, c("code", "name", "category2", "category1", "style1", "style2"))
fund.wind <- fund.wind[, id := substring(code, 1, 6)
	][, .(id, category2)]
#match
SJ <- fund.wind[SJ, on = .(id), nomatch = 0
	]

syf <- SJ[, .(id, year, sem, quarter, month, date, netflow, month_return, ztk)]

zuihou <- syf[, .(id, year, sem, month, ztk)
	][month == 6 | month == 12
	][, .(id, year, sem, ztk)]
setnames(zuihou, "ztk", "s.ztk")
#滞后
zuihou <- zuihou[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]

syf <- zuihou[syf, on = .(id, year, sem)]

a <- syf[order(id, year, month)
	][, month_return.h1 := shift(month_return, n = 1, fill = NA, type = "lead")
	][, tk.group := ntile(s.ztk, 5), keyby = .(year, month)]

#a <- syf[order(id, year, month)
#	][, month_return.h1 := shift(month_return, n = 1, fill = NA, type = "lead")
#	][, tk.group := ntile(ztk, 5), keyby = .(year, month)]

t.test(a[tk.group == 5, netflow])
t.test(a[tk.group == 4, netflow])
t.test(a[tk.group == 5, netflow], a[tk.group == 1, netflow], paired = FALSE)

t.test(a[tk.group == 5, month_return])
t.test(a[tk.group == 1, month_return])
t.test(a[tk.group == 5, month_return.h1], a[tk.group == 1, month_return.h1], paired = FALSE)
wu <- a[tk.group == 5, .(id, date, month_return)
	][, .(m_return.5 = mean(month_return)), keyby = .(date)]
yi <- a[tk.group == 1, .(id, date, month_return)
	][, .(m_return.1 = mean(month_return)), keyby = .(date)]
he <- wu[yi, on = .(date)
	][, dif := m_return.1 - m_return.5]
t.test(he[, dif])






