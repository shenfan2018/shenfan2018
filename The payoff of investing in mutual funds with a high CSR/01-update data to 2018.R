# portfolio
# csamr上的
#读取个股持仓(这里整体选择是年报和中报，且数据从2004.1.1-2019.1.1
pf1 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//个股持仓//2//Fund_Portfolio_Stock1.xlsx")
pf2 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//个股持仓//2//Fund_Portfolio_Stock2.xlsx")
pf3 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//个股持仓//2//Fund_Portfolio_Stock3.xlsx")
pf4 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//个股持仓//2//Fund_Portfolio_Stock4.xlsx")
pf5 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//个股持仓//2//Fund_Portfolio_Stock5.xlsx")
pf <- rbindlist(list(pf1, pf2, pf3, pf4, pf5))
rm(pf1, pf2, pf3, pf4, pf5)
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

portfolio <- portfolio[order(id, date, Rank)]

save(portfolio, file = "portfolio.RData")



## fund NAV
# NAV daily(NAVA为原始，NAV处理) 2004.1.1-2019.1.1
NAV0 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM0406.xlsx")
NAV00 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM0608.xlsx")
NAV000 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM0810.xlsx")
NAV1 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM1.xlsx")
NAV2 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM2.xlsx")
NAV3 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM3.xlsx")
NAV4 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM4.xlsx")
NAV5 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM5.xlsx")
NAV6 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM6.xlsx")
NAV7 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM7.xlsx")
NAV8 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM8.xlsx")
NAV9 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM9.xlsx")
NAV <- rbindlist(list(NAV0, NAV00, NAV000, NAV1, NAV2, NAV3, NAV4, NAV5, NAV6, NAV7, NAV8, NAV9))
rm(NAV0, NAV00, NAV000, NAV1, NAV2, NAV3, NAV4, NAV5, NAV6, NAV7, NAV8, NAV9)
#NAV原始数据
data.NAV <- NAV
data.NAV <- as.data.table(data.NAV)
setnames(data.NAV, 1:2, c("date", "id"))
data.NAV <- data.NAV[date != "没有单位'", .SD
	][date != "统计日期'", .SD
	][, AdjustedNAV := as.numeric(as.character(AdjustedNAV))
	][, AdjustedNAVGrowth := as.numeric(as.character(AdjustedNAVGrowth))
	][, date := as.Date(as.character(date))
	][, c("FullName", "NAV") := NULL]

# 根据adjustedNAV自己算return,这里的收益率都是%，都*100
# 排序
data.NAV <- data.NAV[order(id, date)
	][, AdjustedNAVGrowth := AdjustedNAVGrowth / 100]

save(data.NAV, file = "fund-NAV.RData")


# category
category <- read_excel("C:/Users/shenfan/Desktop/The payoff of investing in mutual funds with a high CSR score portfolio Evidence from China/基金data/基金类型.xlsx")
category <- as.data.table(category)
setnames(category, 1:4, c("code", "name", "category1", "category2"))
category <- category[category2 == "偏股混合型基金" | category2 == "普通股票型基金"
	][, id := substring(code, 1, 6)
	][, c("code", "name") := NULL]




