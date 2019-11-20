#csamr上的
#读取个股持仓(这里整体选择是年报和中报，且数据从2009.1.1-2017.12.31
pf0 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//Fund_Portfolio_Stock0.xlsx")
pf1 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//Fund_Portfolio_Stock.xlsx")
pf2 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//Fund_Portfolio_Stock1.xlsx")
pf3 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//Fund_Portfolio_Stock2.xlsx")
pf <- rbindlist(list(pf0,pf1, pf2, pf3))
rm(pf1, pf2, pf3, pf0)
pf <- as.data.table(pf) #pf为个股持仓原始文件，误删
#将数据变成portfolio
portfolio <- pf
portfolio <- as.data.table(portfolio)
#选择中报和年报
portfolio <- portfolio[ReportTypeID == 5 | ReportTypeID == 6, .SD]
#整理，包括去除不要的列和enddate改名
setnames(portfolio, "EndDate", "date")
setnames(portfolio, "Symbol", "stock.id")
setnames(portfolio, 2, "id")
portfolio <- portfolio[, c("FundID", "ReportTypeID", "Startdate", "InvestmentType") := NULL]
#让proportion等变成数字,这个proportion是占净值比例
portfolio <- portfolio[, Proportion := as.numeric(as.character(Proportion))
	][, Rank := as.numeric(as.character(Rank))
	][, MarketValue := as.numeric(as.character(MarketValue))
	][, Shares := as.numeric(as.character(Shares))
	][, date := as.Date(as.character(date))]

#匹配category
category <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//windstyle.xlsx")
category <- as.data.table(category)
setnames(category, 1:4, c("code", "id", "name","fund.category"))
category <- category[fund.category == "普通股票型基金" | fund.category == "偏股混合型基金", .SD
	][,c("code","id","fund.category","name")]

portfolio <- category[portfolio, on = .(id), nomatch = 0]

portfolio <- portfolio[order(id, date, Rank)
	][, total.MV := sum(MarketValue), keyby = .(id, date)
	][, proportion.stock := MarketValue / total.MV]


save(portfolio, file = "fund-portfolio.RData")



#这里存在很多基金成立时间的问题，我们加入成立时间
# start.date <- read_excel("C://Users//shenfan//Desktop//data//成立日期//Fund_MainInfo.xlsx")
# start.date <- as.data.table(start.date)
# setnames(start.date, 1:2, c("id", "start_date"))
# start.date <- start.date[id != "基金主代码'"
# 	][id != "没有单位'"
# 	][, start_date := as.Date(start_date)]




#162107这只存在问题，8.25重新发行的,这里将8.25当成6.30
# portfolio <- portfolio[id != "162107" | date > "2016-06-30", .SD
#	][date == "2016-08-24", date := as.Date("2016-06-30")]

data <- portfolio



# 验证下来正确的 这部分不需要进入 直接对portfolio进行就可以
# 加入股票投资市值
stockMV1 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//股票型 股票投资市值.xlsx")
stockMV2 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//混合型 股票投资市值.xlsx")
stockMV <- rbindlist(list(stockMV1, stockMV2))
stockMV <- as.data.table(stockMV)
setnames(stockMV, 1:34, c("code", "name", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
stockMV = melt(stockMV, id.vars = c("code", "name"), measure.vars = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
setnames(stockMV, "variable", "date")
setnames(stockMV, 4, c("stock.MV"))
stockMV <- stockMV[, date := as.Date(as.character(date))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD]

data <- stockMV[data, on = .(code, date),nomatch=0]


data <- data[, MV.zj := sum(MarketValue), keyby = .(id, date)
	][, shifou := MV.zj / stock.MV]









#读取stock数据，每日的，包括收盘价和收益率，命名为stock
load("datastock.RData")

# sem.return是下半年的收益率
stock <- stock[order(stock.id,date)
	][, Dretwd2 := 1 + Dretwd
	][, year := year(date)
	][, quarter := quarter(date)
	][, quarter.return := prod(Dretwd2, na.rm = FALSE) - 1, keyby = .(stock.id, year, quarter)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, sem.return := prod(Dretwd2, na.rm = FALSE) - 1, keyby = .(stock.id, year,sem)]

#报告期股票市值



data <- stock[data, on = .(date, stock.id)]

# 验证了一下 MarketValue没错
# data <- data[, MV := Clsprc * Shares]









## 不用了
# 另外数据库，具体持仓，整体有问题
# zcg1 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//基金重仓//基金重仓流通股_1.xlsx")
# zcg2 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//基金重仓//基金重仓流通股_2.xlsx")
# zcg3 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//基金重仓//基金重仓流通股_3.xlsx")
# zcg4 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//基金重仓//基金重仓流通股_4.xlsx")
# zcg.raw <- rbindlist(list(zcg1, zcg2, zcg3, zcg4)) 
# rm(zcg1, zcg2, zcg3, zcg4)
# zcg <- zcg.raw
# zcg <- as.data.table(zcg)
# zcg <- zcg[SCode != "股票代码"]

# fund名字
# fund.name <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//基金重仓//Fund_MainInfo.xlsx")
# fund.name <- as.data.table(fund.name)
# setnames(fund.name, 2:3, c("id", "InstName"))
# fund.name <- fund.name[, c("id", "InstName")]
#匹配
# zcg <- fund.name[zcg, on = .(InstName)]
# zvg <- zcg[, na := ifelse(is.na(id), 1, 0)]
# zvg<-zvg[na==1,.SD]

# zvg <- zvg[order(InstName)
# 	][, unique(InstName)]
