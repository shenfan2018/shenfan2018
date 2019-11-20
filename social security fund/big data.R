# big data
# 股价整理 为2010-2017的日度数据(2003.1.1-2017.12.31)
#stock daily
stock0 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr0.xlsx")
stock1 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr1.xlsx")
stock2 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr2.xlsx")
stock3 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr3.xlsx")
stock4 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr4.xlsx")
stock5 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr5.xlsx")
stock6 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr6.xlsx")
stock7 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr7.xlsx")
stock8 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr8.xlsx")
stock9 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr9.xlsx")
stock.raw <- rbindlist(list(stock0, stock1, stock2, stock3, stock4, stock5, stock6, stock7, stock8, stock9)) #stock原文件 勿删
rm(stock0, stock1, stock2, stock3, stock4, stock5, stock6, stock7, stock8, stock9)
stock <- stock.raw
stock <- as.data.table(stock)
setnames(stock, c("Trddt", "Stkcd"), c("date", "stock.id"))
stock <- stock[, c("stock.id", "date", "Clsprc", "Dretwd", "Dsmvosd", "Dsmvtll")]

#整理
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(Clsprc)
	][, Dretwd := as.numeric(Dretwd)
	][, date := as.Date(date)
	][, Dsmvosd := as.numeric(Dsmvosd)
	][, Dsmvtll := as.numeric(Dsmvtll)]

# 这里有过save了
save(stock, file = "stock.RData")


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


save(portfolio, file = "portfolio.RData")


# stock beta
# beta 过去12个月
# β 个股过去12个月超额收益率回归到市场的超额收益率
load("stock.RData")
# 所有数据应该变成month
stock.m <- stock
stock.m <- stock.m[order(stock.id, date)
	][, month := month(date)
	][, year := year(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_ret = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)]

# 加入mkt_ret
four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, .(year, month, mkt, rf)]

stock.m <- mkt[stock.m, on = .(year, month)]

stock.m <- stock.m[, mkt_rf := mkt - rf
	][, ret_rf := month_ret - rf]

stock.m <- stock.m[, tag := seq(1:(.N)), keyby = stock.id]

# ret <- stock.m[year < 2019]
ret <- stock.m

reg.roll <- list()
for (i in 12:180) {
	reg.roll[[i]] <- ret[tag >= i - 11 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf) %>% coef() %>% as.list()
	},
	keyby = .(stock.id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:180, reg.roll)
roll <- roll[12:180]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
ret <- reg.cof[ret, on = .(tag, stock.id)
	][, beta := mkt_rf
	][, .(stock.id, year, month, beta)]

beta <- ret

save(beta,file = "stockbeta.RData")