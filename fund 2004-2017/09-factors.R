# 各种因子以及alpha
four <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four <- as.data.table(four)
four <- four[, date := as.Date(trddy)]

load("fund-NAV.RData")
##fund style(wind+csmar)
fund.csmar <- read_excel("C://Users//shenfan//Desktop//data//基金类型风格//Fund_MainInfo.xlsx")
fund.csmar <- as.data.table(fund.csmar)
setnames(fund.csmar, 1:2, c("id", "name"))
fund.csmar <- fund.csmar[name != "基金全称'", .SD
	][name != "没有单位'", .SD
	][, name := NULL]

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



data <- data.NAV
data <- four[data, on = .(date), nomatch = 0]

data <- data[, rit := AdjustedNAVGrowth - rf
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][rit != "NA", .SD
	][, alpha_capm := coef(lm(rit ~ mkt_rf))[1], keyby = .(id, year, sem)
	][, alpha_3 := coef(lm(rit ~ mkt_rf + smb + hml))[1], keyby = .(id, year, sem)
	][, alpha_4 := coef(lm(rit ~ mkt_rf + smb + hml + umd))[1], keyby = .(id, year, sem)
	][, alpha_5 := coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma))[1], keyby = .(id, year, sem)]


# 变成变年度
data <- data[, yesno := duplicated(alpha_capm), keyby = .(id, year, sem)
	][yesno == FALSE, .SD
	][, c("id", "year", "sem", "alpha_capm", "alpha_3", "alpha_4", "alpha_5")]

load("fund7.RData")

fund.8 <- data[fund.7, on = .(id, year, sem), nomatch = 0]
save(fund.8, file = "fund8.RData")




