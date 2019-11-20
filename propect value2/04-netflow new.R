load("fundPV-st1.RData")

fund.PV <- fund.PV[, quarter := quarter(date)
	][, .(TK = mean(TK), PW = mean(PW), LA = mean(LA), CC = mean(CC)), keyby = .(id, year, quarter)]

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

data <- fundflow[fund.PV, on = .(id, year, quarter), nomatch = 0]

## clean
#极端值处理
#对netflow
a1 <- quantile(data[, netflow], 0.99, na.rm = TRUE)
a2 <- quantile(data[, netflow], 0.01, na.rm = TRUE)
data <- data[order(netflow)
	][, extremum := ifelse(netflow > a1, 2, ifelse(netflow < a2, 1, 0))
	][extremum == 1, netflow := netflow[.N]
	][extremum == 2, netflow := netflow[1]]


# 差分
data <- data[order(id, year, quarter)
	][, TK.c := c(NA, diff(TK, difference = 1)), keyby = id
	][, PW.c := c(NA, diff(PW, difference = 1)), keyby = id
	][, LA.c := c(NA, diff(LA, difference = 1)), keyby = id
	][, CC.c := c(NA, diff(CC, difference = 1)), keyby = id
	][, netflow.1 := shift(netflow, n = 1, fill = NA, type = "lag"), keyby = id]


############################################################################### 当期netflow对TK
data2 <- data[!is.na(TK)
	][!is.na(netflow)]

TK.g <- data2[, netflow.g := ntile(netflow, 5), keyby = .(year, quarter)
	][, .(TK = mean(TK)), keyby = .(year, quarter, netflow.g)]

low <- TK.g[netflow.g == 1]
high <- TK.g[netflow.g == 5]
dif <- low[high, on = .(year, quarter)
	][, dif := i.TK - TK]

t.test(TK.g[netflow.g == 5, TK])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

############################################################################### 前一期net flow对TK
data2 <- data[!is.na(TK)
	][!is.na(netflow.1)]

TK.g <- data2[, netflow.g := ntile(netflow.1, 5), keyby = .(year, quarter)
	][, .(TK = mean(CC)), keyby = .(year, quarter, netflow.g)]

low <- TK.g[netflow.g == 1]
high <- TK.g[netflow.g == 5]
dif <- low[high, on = .(year, quarter)
	][, dif := i.TK - TK]

t.test(TK.g[netflow.g == 5, TK])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


############################################################################### 当期netflow对TK change
data2 <- data[!is.na(TK.c)
	][!is.na(netflow)]

TK.g <- data2[, netflow.g := ntile(netflow, 5), keyby = .(year, quarter)
	][, .(TK.c = mean(TK.c)), keyby = .(year, quarter, netflow.g)]

low <- TK.g[netflow.g == 1]
high <- TK.g[netflow.g == 5]
dif <- low[high, on = .(year, quarter)
	][, dif := i.TK.c - TK.c]

t.test(TK.g[netflow.g == 5, TK.c])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


##############################################################################
# 上一期netflow对TK change
data2 <- data[!is.na(TK.c)
	][!is.na(netflow.1)]

TK.g <- data2[, netflow.g := ntile(netflow.1, 5), keyby = .(year, quarter)
	][, .(TK.c = mean(CC.c)), keyby = .(year, quarter, netflow.g)]

low <- TK.g[netflow.g == 1]
high <- TK.g[netflow.g == 5]
dif <- low[high, on = .(year, quarter)
	][, dif := i.TK.c - TK.c]

t.test(TK.g[netflow.g == 5, TK.c])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

