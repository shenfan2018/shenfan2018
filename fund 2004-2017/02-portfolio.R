#csamr上的
#读取个股持仓(这里整体选择是年报和中报，且数据从2004.1.1-2018.1.1
pf1 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock1.xlsx")
pf2 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock2.xlsx")
pf3 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock3.xlsx")
pf4 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock4.xlsx")
pf <- rbindlist(list(pf1,pf2, pf3, pf4))
rm(pf1, pf2, pf3, pf4)
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
setnames(category, 1:4, c("code", "id", "name", "fund.category"))
category <- category[fund.category == "普通股票型基金" | fund.category == "偏股混合型基金", .SD
	][, c("code", "id", "fund.category", "name")]

portfolio <- category[portfolio, on = .(id), nomatch = 0]

portfolio <- portfolio[order(id, date, Rank)
	][, total.MV := sum(MarketValue), keyby = .(id, date)
	][, proportion.stock := MarketValue / total.MV]


save(portfolio, file = "fund-portfolio.RData")
