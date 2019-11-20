# 这个做figure的
# low low的基金的股票
load("fund9.RData")
data <- fund.9
data <- data[order(year, sem, category2, - sem_return.1)
	][, rank.1 := sequence(.N), by = .(year, sem, category2)]

# 这里啊 要把NA的去掉哦
data <- data[, na := ifelse(is.na(sem_return.1), 1, 0)]
# 这里把sem_return.1缺失的都删掉了
data <- data[na == 0, .SD
	][, per.tb := ntile(rank.1, 5), keyby = .(year, sem)]
data <- data[per.tb == 1, flow.tb := ntile(netflow.1, 5)
	][per.tb == 2, flow.tb := ntile(netflow.1, 5)
	][per.tb == 3, flow.tb := ntile(netflow.1, 5)
	][per.tb == 4, flow.tb := ntile(netflow.1, 5)
	][per.tb == 5, flow.tb := ntile(netflow.1, 5)]

lowlow <- data[per.tb == 5 & flow.tb == 1, .SD
	][, L.net := quantile(fund.risk.c, 0.2, na.rm = TRUE), by = DateQ
	][, H.net := quantile(fund.risk.c, 0.8, na.rm = TRUE), by = DateQ
	][, group := ifelse(fund.risk.c < L.net, 1, ifelse(fund.risk.c > H.net, 3, 2))
	][, c("id", "year", "sem", "group")]

lowlow <- lowlow[, low := 1]


# 各种因子以及alpha，
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
	][, month := month(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, return.m := prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1, keyby = .(id, year, month)
	][rit != "NA", .SD
	][, alpha_capm := coef(lm(rit ~ mkt_rf))[1], keyby = .(id, year, month)
	][, alpha_3 := coef(lm(rit ~ mkt_rf + smb + hml))[1], keyby = .(id, year, month)
	][, alpha_4 := coef(lm(rit ~ mkt_rf + smb + hml + umd))[1], keyby = .(id, year, month)
	][, alpha_5 := coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma))[1], keyby = .(id, year, month)]

# 变成月度度
data <- data[, yesno := duplicated(return.m), keyby = .(id, year, month)
	][yesno == FALSE, .SD
	][, c("id", "year", "month", "sem", "return.m", "alpha_capm", "alpha_3", "alpha_4", "alpha_5")]

# 选出lowlow
data <- lowlow[data, on = .(id, year, sem)]

# 往下6期
data <- data[order(id, year, month)
	][, low.d1 := shift(low, n = 6, fill = NA, type = "lag"), keyby = id
	][, group.d1 := shift(group, n = 6, fill = NA, type = "lag"), keyby = id
	][group.d1 == 1 | group.d1 == 2, group := group.d1]

# 选出来本期和后期的
data.f <- data[low.d1 == 1 | low == 1, .SD]

data.f <- data.f[order(id, year, month)
	][, period := seq(from = 1, to = 12)
	][, c("period", "return.m", "alpha_capm", "alpha_3", "alpha_4", "alpha_5", "group")]

figure <- data.f[, .(return.m = mean(return.m),alpha_capm = mean(alpha_capm), alpha_3 = mean(alpha_3), alpha_4 = mean(alpha_4), alpha_5 = mean(alpha_5)), keyby = .(period, group)]

# 是否6期还是12期
medium <- figure[group == 2 , .SD]
high <- figure[group == 3 , .SD]
low <- figure[group == 1 , .SD]


ggplot(high, aes(x = period)) +
	geom_line(aes(y = alpha_3, colour = "var1")) +
	geom_line(aes(y = alpha_4, colour = "var2")) +
	geom_line(aes(y = alpha_5, colour = "var3"))

ggplot(low, aes(x = period)) +
	geom_line(aes(y = alpha_3, colour = "var1")) +
	geom_line(aes(y = alpha_4, colour = "var2")) +
	geom_line(aes(y = alpha_5, colour = "var3"))

ggplot(medium, aes(x = period)) +
	geom_line(aes(y = alpha_3, colour = "var1")) +
	geom_line(aes(y = alpha_4, colour = "var2")) +
	geom_line(aes(y = alpha_5, colour = "var3"))





for (i in 1:6) {
	data <- data[, str_c(colnames(data)[4:7], str_c(".d", i)) := lapply(.SD[, 3:6], shift, n = i, fill = NA, type = "lead"), keyby = id]
}

