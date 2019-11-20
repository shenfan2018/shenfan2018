# new single sorting 加上proportion
load("fundPV-st1.RData")

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

# 
data <- ret[fund.PV, on = .(id, year, month)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, date, year, sem, month, month_return, TK, PW, LA, CC)]


#加入csmar的股票市值占基金资产净值比
stockpro <- read_excel("C://Users//shenfan//Desktop//data//2//股票市值占基金资产净值比.xlsx")
stockpro <- as.data.table(stockpro)
setnames(stockpro, 1:30, c("code", "name", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
stockpro = melt(stockpro, id.vars = c("code", "name"), measure.vars = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))

setnames(stockpro, c("value", "variable"), c("stock.proportion", "DateQ"))
stockpro <- stockpro[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, id := substring(code, 1, 6)
	][quarter == 2, sem := 1
	][quarter == 4, sem := 2
	][, c("id", "year", "sem", "stock.proportion")]

# 这里还可以讨论一下 这个stockpro是否要滞后
stockpro <- stockpro[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]

data <- stockpro[data, on = .(id, year, sem)]

data <- data[, TK := TK * 0.01 * stock.proportion
	][, PW := PW * 0.01 * stock.proportion
	][, LA := LA * 0.01 * stock.proportion
	][, CC := CC * 0.01 * stock.proportion]

#  常规single sorting
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, PW.g := ntile(PW, 5), keyby = .(date)
	][, LA.g := ntile(LA, 5), keyby = .(date)
	][, CC.g := ntile(TK, 5), keyby = .(date)]

############################# TK
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[TK.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)