national <- read_excel("C:/Users/shenfan/Desktop/社保/data/国家队重仓流通股.xlsx")
national <- as.data.table(national)
setnames(national, 1:11, c("code", "stock.name", "national.name", "date", "realtime", "hold.T", "changes", "hold.T-1", "mv.T", "mv.change", "mv.T-1"))

# 2015 二季度到三季度
buy <- national[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][year == 2015 & quarter == 3
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, national.name, year, quarter, mv.T, mv.change)]

buy.stock <- buy[, .(times = (.N), mv.total = sum(mv.T)), keyby = .(stock.id)
	][order(-mv.total, - times)]

## 社保
# all sample stocks on the basis of changes in IO
time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")

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

load("stock.RData")
stock.q <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, .SD[.N], keyby = .(stock.id, year, quarter)
	][, .(stock.id, year, quarter, Dsmvosd)]

IOS <- stock.q[IOS, on = .(stock.id, year, quarter)
	][, mv := IO.social * Dsmvosd]

# 沪深300
## 沪深300成分股
CSI <- read_excel("C://Users//shenfan//Desktop//社保//data//沪深300成分股.xlsx")
CSI <- as.data.table(CSI)
CSI <- CSI[, .(stock.id, date)
	][, DateQ := as.Date(date)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, CSI := 1
	][, .(stock.id, year, quarter, CSI)]

IOS <- CSI[IOS, on = .(stock.id, year, quarter)]

# 社保持仓 2015年第二季度
social.hold <- IOS[year == 2015 & quarter == 2
	][!is.na(IO.social)
	]
	# [is.na(CSI)]

# 看社保持仓与国家队加仓的重合度
same <- buy.stock[social.hold, on = .(stock.id)
	][, mv.ntile := ntile(mv, 10)
	][, yesno := ifelse(is.na(times), 0, 1)]

# 总
sum(same[, yesno]) / same[, .N] # 263, 575 0.4573913

# 百分比 
sum(same[mv.ntile == 10, yesno]) / same[mv.ntile == 10, .N] # 389 708 54.94%


# 社保持仓 2015年第三季度
social.hold <- IOS[year == 2015 & quarter == 3
	][!is.na(IO.social)]

# 看社保持仓与国家队加仓的重合度
same <- buy.stock[social.hold, on = .(stock.id)
	][, yesno := ifelse(is.na(times), 0, 1)
	]
sum(same[, yesno]) # 324  665  48.72%

	

