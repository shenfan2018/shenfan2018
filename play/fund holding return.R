#csamr上的
#读取个股持仓(这里整体选择是年报和中报，且数据从2004.1.1-2018.1.1
pf1 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock1.xlsx")
pf2 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock2.xlsx")
pf3 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock3.xlsx")
pf4 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock4.xlsx")
pf <- rbindlist(list(pf1, pf2, pf3, pf4))
rm(pf1, pf2, pf3, pf4)
pf <- as.data.table(pf) #pf为个股持仓原始文件，误删
#将数据变成portfolio
portfolio <- pf
portfolio <- as.data.table(portfolio)
#选择中报和年报
portfolio <- portfolio[ReportTypeID == 1 | ReportTypeID == 2 | ReportTypeID == 3 | ReportTypeID == 4, .SD]
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
setnames(category, 1:4, c("code", "id", "name", "fund.category"))
category <- category[fund.category == "普通股票型基金" | fund.category == "偏股混合型基金", .SD
	][, c("code", "id", "fund.category", "name")]

portfolio <- category[portfolio, on = .(id), nomatch = 0]

portfolio <- portfolio[order(id, date, Rank)
	][, total.MV := sum(MarketValue), keyby = .(id, date)
	][, proportion.stock := MarketValue / total.MV]

write.csv(portfolio, "C://Users//shenfan//Desktop//portfolio.csv")

portfolio <- portfolio[, year := year(date)
	][, month := month(date)]


## 匹配股票
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund/stocklhb.RData")
stock.p <- stock
stock.p <- stock.p[order(stock.id, date)
	][, Dretwd2 := Dretwd + 1
	][, month := month(date)
	][, year := year(date)
	][, .(ret = prod(Dretwd2, na.rm = TRUE) - 1), keyby = .(stock.id, year, month)
	]

stock.p <- stock.p[, ret.1 := shift(ret, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)
	][, ret.2 := shift(ret, n = 2, fill = NA, type = "lead"), keyby = .(stock.id)
	][, ret.3 := shift(ret, n = 3, fill = NA, type = "lead"), keyby = .(stock.id)]

# match
a <- stock.p[portfolio, on = .(stock.id, year, month)
	][, return1 := ret.1 * proportion.stock
	][, return2 := ret.2 * proportion.stock
	][, return3 := ret.3 * proportion.stock
	][, .(return.1 = sum(return1, na.rm = TRUE), return.2 = sum(return2, na.rm = TRUE), return.3 = sum(return3, na.rm = TRUE)), keyby = .(id, name, date)]
a <- melt(a, id.vars = c("id", "name", "date"), measure.vars = c("return.1", "return.2", "return.3"))
a <- a[, year := year(date)
	][, month := month(date)
	][, variable := as.character(variable)
	][variable == "return.1", month := month + 1
	][variable == "return.2", month := month + 2
	][variable == "return.3", month := month + 3
	][month == 13 | month == 14 | month == 15, year := year + 1
	][month == 13, month := 1
	][month == 14, month := 2
	][month == 15, month := 3
	][order(id, year, month)
	][, .(id, name, date, year, month, value)]

setnames(a, "value", "ret")

write.csv(a, "C://Users//shenfan//Desktop//fund.csv")

