# Add a dummy variable to define that whether the stock is held by social fund in Sep 2015 and no longer held (holding decrease) in Dec 2015
## ROA, Market to book ratio, dividend ratio, SOE dummy, blue-chip dummy 
# 取国家队 2015年 第四季度卖出的，看是否社保2015年第三季度持有但是第四季度卖出了（或者减仓）
national <- read_excel("C:/Users/shenfan/Desktop/社保/data/国家队重仓流通股.xlsx")
national <- as.data.table(national)
setnames(national, 1:11, c("code", "stock.name", "national.name", "date", "realtime", "hold.T", "changes", "hold.T-1", "mv.T", "mv.change", "mv.T-1"))

# 2015 三季度到四季度
sell <- national[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][year == 2015 & quarter == 4
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, national.name, mv.change)]

# 主表
sell.stock <- sell[, .(mv.total = sum(mv.change)), keyby = .(stock.id)
	][mv.total < 0
	][, sell := 1]

time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")
# ROA
ROA <- read_excel("C:/Users/shenfan/Desktop/社保/data/ROA.xlsx")
ROA <- as.data.table(ROA)
setnames(ROA, 1:58, c("code", "name", time))
ROA = melt(ROA, id.vars = c("code", "name"), measure.vars = time)
setnames(ROA, c("value", "variable"), c("ROA", "DateQ"))

ROA <- ROA[, DateQ := as.Date(as.character(DateQ))
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][year == 2015 & quarter == 3
	][, .(stock.id, ROA)]

# match
exit <- ROA[-(.N)
	][-(.N)]

##DP 股息率是近12个月分配给股东的股息占股价的百分比。
DP <- read_excel("C://Users//shenfan//Desktop//社保//data//dividend yield.xlsx")
DP <- as.data.table(DP)
setnames(DP, 1:60, c("stock", "name", "2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30"))
DP = melt(DP, id.vars = c("stock", "name"), measure.vars = c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30"))
setnames(DP, c("value", "variable"), c("DP", "DateQ"))

DP <- DP[, DateQ := as.Date(as.character(DateQ))
	][order(stock, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(stock, 1, 6)
	][year == 2015 & quarter == 3
	][, .(stock.id, DP)]

# match
exit <- DP[exit, on = .(stock.id)]

# BM 股东权益/流通市值
BM <- read_excel("C:/Users/shenfan/Desktop/社保/data/BM/FI_T10.xlsx")
BM <- as.data.table(BM)
setnames(BM, 1:3, c("stock.id", "date", "BM"))
BM <- BM[stock.id != "没有单位'", .SD
	][stock.id != "股票代码'", .SD
	][, date := as.Date(date)
	][, BM := as.numeric(BM)
	][, year := year(date)
	][, quarter := quarter(date)
	][year == 2015 & quarter == 3
	][, .(stock.id, BM)]

# match
exit <- BM[exit, on = .(stock.id)]

# leverage
asset <- read_excel("C:/Users/shenfan/Desktop/社保/data/total asset.xlsx")
asset <- as.data.table(asset)
setnames(asset, 1:58, c("code", "name", time))
asset = melt(asset, id.vars = c("code", "name"), measure.vars = time)
setnames(asset, c("value", "variable"), c("total.asset", "DateQ"))
liability <- read_excel("C:/Users/shenfan/Desktop/社保/data/total liabilities.xlsx")
liability <- as.data.table(liability)
setnames(liability, 1:58, c("code", "name", time))
liability = melt(liability, id.vars = c("code", "name"), measure.vars = time)
setnames(liability, c("value", "variable"), c("total.liability", "DateQ"))
asset <- liability[asset, on = .(code, name, DateQ)]

leverage <- asset[, DateQ := as.Date(as.character(DateQ))
	][order(code, DateQ)
	][, leverage := total.liability / total.asset
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][year == 2015 & quarter == 3
	][, .(stock.id, leverage)]

# match
exit <- leverage[exit, on = .(stock.id)]

# SOE 企业属性
SOE <- read_excel("C:/Users/shenfan/Desktop/社保/data/企业属性.xlsx")
SOE <- as.data.table(SOE)
setnames(SOE, 1:3, c("code", "name", "xz"))
SOE <- SOE[, SOE := ifelse(xz == "地方国有企业" | xz == "中央国有企业", 1, 0)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, SOE)]

# match
exit <- SOE[exit, on = .(stock.id)]

## export
export <- read_excel("C:/Users/shenfan/Desktop/社保/data/海外收入.xlsx")
export <- as.data.table(export)
setnames(export, 1:58, c("code", "name", time))
export = melt(export, id.vars = c("code", "name"), measure.vars = time)
setnames(export, c("value", "variable"), c("foreign", "DateQ"))

export <- export[, DateQ := as.Date(as.character(DateQ))
	][order(code, DateQ)
	][, export := ifelse(is.na(foreign), 0, 1)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][year == 2015 & quarter == 3
	][, .(stock.id, export)]

# match
exit <- export[exit, on = .(stock.id)]

## CF net operating cash flow/ total asset
asset <- read_excel("C:/Users/shenfan/Desktop/社保/data/total asset.xlsx")
asset <- as.data.table(asset)
setnames(asset, 1:58, c("code", "name", time))
asset = melt(asset, id.vars = c("code", "name"), measure.vars = time)
setnames(asset, c("value", "variable"), c("total.asset", "DateQ"))
OCF <- read_excel("C:/Users/shenfan/Desktop/社保/data/经营活动产生的现金流量净额.xlsx")
OCF <- as.data.table(OCF)
setnames(OCF, 1:58, c("code", "name", time))
OCF = melt(OCF, id.vars = c("code", "name"), measure.vars = time)
setnames(OCF, c("value", "variable"), c("OCF", "DateQ"))

CF <- OCF[asset, on = .(code, name, DateQ)]
CF <- CF[, DateQ := as.Date(as.character(DateQ))
	][order(code, DateQ)
	][, CF := OCF / total.asset
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][year == 2015 & quarter == 3
	][, .(stock.id, CF)]

# match
exit <- CF[exit, on = .(stock.id)]

## CSI
CSI <- read_excel("C://Users//shenfan//Desktop//社保//data//沪深300成分股.xlsx")
CSI <- as.data.table(CSI)
CSI <- CSI[, .(stock.id, date)
	][, DateQ := as.Date(date)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, CSI := 1
	][year == 2015 & quarter == 3
	][, .(stock.id, CSI)]

exit <- CSI[exit, on = .(stock.id)]
exit <- exit[, CSI := ifelse(is.na(CSI), 0, 1)]

save(exit, file = "exit.RData")

load("exit.RData")
exit<- sell.stock[exit, on = .(stock.id)
	][, sell := ifelse(is.na(sell), 0, 1)]


# 社保
IOS <- read_excel("C://Users//shenfan//Desktop//社保//data//社保基金持股比例.xlsx")
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IO.social", "DateQ"))

IOS <- IOS[, DateQ := as.Date(as.character(DateQ))
	][order(code, DateQ)
	][, IO.social := ifelse(is.na(IO.social), 0, IO.social)
	][, IO.social.2 := shift(IO.social, n = 1, fill = NA, type = "lead"), keyby = .(code)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][year == 2015 & quarter == 3
	][, sell.s := ifelse(IO.social > IO.social.2, 1, 0)
	][, .(stock.id, sell.s)]


exit <- IOS[exit, on = .(stock.id)]

# 删除CSI
del <- exit[CSI == 0]

glm(sell ~ sell.s + CF + export + SOE + BM + leverage + DP + ROA, family = binomial(link = "probit"), data = del) %>% summary()

glm(sell ~ sell.s + CF + export + SOE +leverage, family = binomial(link = "probit"), data = del) %>% summary()