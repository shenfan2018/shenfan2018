# fund beta 的计算
load("fund-NAV.RData")

##fund style(wind+csmar)
fund.csmar <- read_excel("C://Users//shenfan//Desktop//data//基金类型风格//Fund_MainInfo.xlsx")
fund.csmar <- as.data.table(fund.csmar)
setnames(fund.csmar, 1:2, c("id", "name"))
fund.csmar <- fund.csmar[name != "基金全称'", .SD][
    name != "没有单位'", .SD][
	, name := NULL]

fund.wind <- read_excel("C://Users//shenfan//Desktop//data//基金类型风格//windstyle.xlsx")
fund.wind <- as.data.table(fund.wind)
setnames(fund.wind, 1:6, c("code", "name", "category2", "category1", "style1", "style2"))
fund.wind <- fund.wind[, id := substring(code, 1, 6)][
    , c("style1", "style2", "name", "code", "category1") := NULL]
fund.category <- fund.csmar[fund.wind, on = .(id)]
#match
data.NAV <- fund.category[data.NAV, on = .(id), nomatch = 0]

#选择混合型和股票型
data.NAV <- data.NAV[Category == "股票型基金" | Category == "混合型基金", .SD
	][category2 == "偏股混合型基金" | category2 == "普通股票型基金", .SD]


# riskfree
riskfree <- read_excel("C://Users//shenfan//Desktop//data//CAPM//risk-free-1.xls")
riskfree <- as.data.table(riskfree)
setnames(riskfree, 1, "date")
setnames(riskfree, 2, "risk_free")
riskfree <- riskfree[, date := as.Date(date)
	][, risk_free.daily := (risk_free + 1) ^ (1 / 365) - 1]

riskfree <- riskfree[, c("date", "risk_free.daily")]
#riskfree匹配上主表
data.NAV <- riskfree[data.NAV, on = .(date), nomatch = 0]


#market risk
marketrisk <- read_excel("C://Users//shenfan//Desktop//data//CAPM//综合日市场回报率文件2004-2018.6//TRD_Cndalym.xlsx")
marketrisk <- as.data.table(marketrisk)
#选择type
marketrisk <- marketrisk[Markettype == "5"][, Markettype := NULL]

setnames(marketrisk, 2, "market_return") #考虑现金红利的综合日市场回报率(流通市值加权平均法)'
setnames(marketrisk, 1, "date")
marketrisk <- marketrisk[date != "没有单位'", .SD][date != "交易日期'", .SD][
    , date := as.Date(date)][
	, market_return := as.numeric(as.character(market_return))]

#匹配market risk
data.NAV <- marketrisk[data.NAV, on = .(date), nomatch = 0]



data.NAV <- data.NAV[, riskpreimum := market_return - risk_free.daily
	][, rit := AdjustedNAVGrowth - risk_free.daily]


# 回归残差 fund.beta
data.NAV <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][AdjustedNAVGrowth != "NA", .SD
	][, rd := residuals(lm(rit ~ riskpreimum)), keyby = .(id, year, sem)
	][, alpha_capm := coef(lm(rit ~ riskpreimum))[1], keyby = .(id, year, sem)
	][, beta_capm := coef(lm(rit ~ riskpreimum))[2], keyby = .(id, year, sem)
	][, risk_capm := sd((rd), na.rm = TRUE), keyby = .(id, year, sem)]

data.NAVQ <- data.NAV

#删除重复项(也就是daily变成quarter)
data.NAVQ <- data.NAVQ[, yesno := duplicated(beta_capm), keyby = .(id, year, sem)
	][yesno == FALSE, .SD
	][, c("id", "year", "sem", "beta_capm")]

load("fund2.RData")
fund.3 <- data.NAVQ[fund.2, on = .(id, year, sem), nomatch = 0]
#可以看见fund.2有8950但是data.NAVQ又9640,这里将不满一个半年的数据是剔除的，如000001，flow数据只有第二季度开始的，所以整个第一个半年是不存在的

save(fund.3, file = "fund3.RData")