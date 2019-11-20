# social为主变量

# 原始stock
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/stock.RData")
#整理
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(Clsprc)
	][, Dretwd := as.numeric(Dretwd)
	][, date := as.Date(date)
	][, Dsmvtll := as.numeric(Dsmvtll) * 1000]

save(stock, file = "stock.RData") # 2003-2017

# MKTCAP,PRC （市值和价格）
load("stock.RData")
social <- stock[, .(stock.id, date, Clsprc, Dsmvtll)
	][order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(PRC = Clsprc[.N], MKTCAP = Dsmvtll[.N]), keyby = .(stock.id, year, quarter)
	][year > 2003, .SD]
# match (MKTCAP,PRC)
social<-social

# DP 股息率是近12个月分配给股东的股息占股价的百分比。
DP <- read_excel("C://Users//shenfan//Desktop//社保//data//dividend yield.xlsx")
DP <- as.data.table(DP)
setnames(DP, 1:60, c("stock", "name", "2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30"))
DP = melt(DP, id.vars = c("stock", "name"), measure.vars = c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30"))
setnames(DP, c("value", "variable"), c("DP", "DateQ"))

DP <- DP[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(stock, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(stock, 1, 6)
	][, .(stock.id, stock, year, quarter, DateQ, DP)]
#match 有8 varibales
social <- DP[social, on = .(stock.id, year, quarter), nomatch = 0]


# age 自从上市以后的月数
age <- read_excel("C://Users//shenfan//Desktop//社保//data//上市日.xlsx")
age <- as.data.table(age)
setnames(age, 1:3, c("stock", "name", "startdate"))
social <- age[social, on = .(stock), nomatch = 0]
social <- social[, enddate := DateQ]
#计算age
social <- social[, days := difftime(enddate, startdate, units = c("days"))
	][, AGE := days / (365/12)
	][, AGE := as.numeric(AGE)
	][, c("days", "startdate", "enddate") := NULL]
# 目前共有10个变量


# return t-3 t
load("stock.RData")
stockreturn <- stock[, .(stock.id, date, Dretwd)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, Dretwd2 := 1 + Dretwd
	][, q_return := prod(Dretwd2, na.rm = TRUE) - 1, keyby = .(stock.id, year, quarter)
	][, tag := (year - 2003) * 4 + quarter
	][, m_return := prod(Dretwd2, na.rm = TRUE) - 1, keyby = .(stock.id, year, month)]
ret3 <- stockreturn[, .(q_return = unique(q_return)), keyby = .(stock.id, year, quarter)]
# match 11个变量
social <- ret3[social, on = .(stock.id, year, quarter), nomatch = 0]


# VOL past 24 monthly stock return overlap
stockvol <- stockreturn[, .(stock.id, year, quarter, month, m_return)
	][, .(m_return = unique(m_return)), keyby = .(stock.id, year, quarter, month)
	][, tag := seq(1:180), keyby = .(stock.id)]

reg.roll <- list()
for (i in 24:180) {
	reg.roll[[i]] <- stockvol[tag >= i - 23 & tag <= i, {
		I <- sd(m_return) %>% as.list()
	},
	keyby = .(stock.id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:180, reg.roll)
roll <- roll[24:180]

reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

vol <- reg.cof[stockvol, on = .(tag, stock.id)
	][, vol := V1
	][month == 3 | month == 6 | month == 9 | month == 12, .SD
	][, .(stock.id, year, quarter, vol)]

# match 12个变量
social <- vol[social, on = .(stock.id, year, quarter), nomatch = 0]


# overlap ret past 12 months
stockret <- stockreturn[, .(stock.id, year, quarter, month, m_return)
	][, .(m_return = unique(m_return)), keyby = .(stock.id, year, quarter, month),
	][, m_return2 := m_return + 1
	][, tag := seq(1:180), keyby = .(stock.id)]

reg.roll <- list()
for (i in 12:180) {
	reg.roll[[i]] <- stockret[tag >= i - 11 & tag <= i, {
		I <- prod(m_return2, na.rm = TRUE) %>% as.list()
	},
	keyby = .(stock.id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:180, reg.roll)
roll <- roll[12:180]

reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

ret12 <- reg.cof[stockret, on = .(tag, stock.id)
	][, ret12 := V1 - 1
	][month == 3 | month == 6 | month == 9 | month == 12, .SD
	][, .(stock.id, year, quarter, ret12)]

# match 13个变量
social <- ret12[social, on = .(stock.id, year, quarter), nomatch = 0]

#保存一下
save(social, file = "variable.RData")

load("variable.RData")
# turnover monthly turnover of past 3 months
turnover <- read_excel("C://Users//shenfan//Desktop//社保//data//月平均换手率.xlsx")
turnover <- as.data.table(turnover)
setnames(turnover, 1:3, c("stock.id", "date", "turnover"))
turnover <- turnover[stock.id != "证券代码'", .SD
	][stock.id != "没有单位'", .SD
	][, turnover := as.numeric(turnover)]
turnover <- turnover[order(stock.id, date)
	][, date := as.Date(date)
	][, quarter := quarter(date)
	][, year := year(date)
	][, .(turnover = mean(turnover)), keyby = .(stock.id, year, quarter)]
# match turnover 14个变量
social <- turnover[social, on = .(stock.id, year, quarter), nomatch = 0]


time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")

## 基金持股比例
IOF <- read_excel("C://Users//shenfan//Desktop//社保//data//基金持股比例.xlsx")
IOF <- as.data.table(IOF)
setnames(IOF, 1:58, c("code", "name", time))
IOF = melt(IOF, id.vars = c("code", "name"), measure.vars = time)
setnames(IOF, c("value", "variable"), c("IO.Fund", "DateQ"))

IOF <- IOF[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, quarter, IO.Fund)]

#match 有15 varibales
social <- IOF[social, on = .(stock.id, year, quarter), nomatch = 0]

## 社保基金持股比例
IOS <- read_excel("C://Users//shenfan//Desktop//社保//data//社保基金持股比例.xlsx")
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IO.social", "DateQ"))

IOS <- IOS[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, quarter, IO.social)]

#match 有15 variables
social <- IOS[social, on = .(stock.id, year, quarter), nomatch = 0]

## 沪深300成分股
CSI <- read_excel("C://Users//shenfan//Desktop//社保//data//沪深300成分股.xlsx")
CSI <- as.data.table(CSI)
CSI <- CSI[, .(stock.id, date)
	][, DateQ := as.Date(date)
	][, CSI := 1
	][, .(stock.id, DateQ, CSI)]

# match 有16 variables
social <- CSI[social, on = .(stock.id, DateQ)
	][, CSI := ifelse(is.na(CSI), 0, 1)]


social.reg <- plm(IO.social ~ ret12 + turnover + vol + q_return + DP + PRC + MKTCAP + AGE + CSI, social, model = "within", effect = "twoways", index = c("stock.id", "DateQ"))

fund.reg <- plm(IO.Fund ~ ret12 + turnover + vol + q_return + DP + PRC + MKTCAP + AGE + CSI, social, model = "within", effect = "twoways", index = c("stock.id", "DateQ"))

stargazer(social.reg, fund.reg, type = "html", out = "C:/Users/shenfan/Desktop/社保/preference.doc")
