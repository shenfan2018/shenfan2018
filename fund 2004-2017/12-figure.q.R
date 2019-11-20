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
	][, L.net := quantile(fund.risk.c, 0.3, na.rm = TRUE), by = DateQ
	][, H.net := quantile(fund.risk.c, 0.7, na.rm = TRUE), by = DateQ
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
	][, return.m := prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1, keyby = .(id, year, quarter)
	][rit != "NA", .SD
	][, alpha_capm := coef(lm(rit ~ mkt_rf))[1], keyby = .(id, year, quarter)
	][, alpha_3 := coef(lm(rit ~ mkt_rf + smb + hml))[1], keyby = .(id, year, quarter)
	][, alpha_4 := coef(lm(rit ~ mkt_rf + smb + hml + umd))[1], keyby = .(id, year, quarter)
	][, alpha_5 := coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma))[1], keyby = .(id, year, quarter)]

# 变成季度度
data <- data[, yesno := duplicated(return.m), keyby = .(id, year, quarter)
	][yesno == FALSE, .SD
	][, c("id", "year", "quarter", "sem", "return.m", "alpha_capm", "alpha_3", "alpha_4", "alpha_5")]

# 选出lowlow
data <- lowlow[data, on = .(id, year, sem)]

# 往下2期和往上一期
data <- data[order(id, year, quarter)
	][, low.1 := shift(low, n = 1, fill = NA, type = "lead"), keyby = id
	][, group.1 := shift(group, n = 1, fill = NA, type = "lead"), keyby = id
	][, low.d1 := shift(low, n = 2, fill = NA, type = "lag"), keyby = id
	][, group.d1 := shift(group, n = 2, fill = NA, type = "lag"), keyby = id
	][, yesno := low.1 + low.d1]

a <- data[low.1 == 1 | low.d1 == 1, low := 1
	][low == 1, .SD]

write.csv(a, "C://Users//shenfan//Desktop//low.csv")

a <- read_excel("C://Users//shenfan//Desktop//lowlow.xlsx")
a <- as.data.table(a)
a <- a[order(id, year, quarter)
	][group.1 == 1 | group.1 == 2 | group.1 == 3, group := group.1
	][group.d1 == 1 | group.d1 == 2 | group.d1 == 3, group := group.d1]

data.f <- a[order(id, year, quarter)
	][, period := seq(from = 0, to = 4), keyby = .(id)]

# 选出来本期和后期的
#data.f <- data[low.d1 == 1 | low == 1 | low.1 == 1, .SD]


# data.f <- data.f[order(id, year, quarter)
#	][low.1 == 1 & is.na(low), period := 0
#	][, one := shift(period, 1, type = "lag")
#	][, two := shift(period, 2, type = "lag")
#	][, three := shift(period, 3, type = "lag")
#	][, four := shift(period, 4, type = "lag")
#	][one == 0, period := 1
#	][two == 0, period := 2
#	][three == 0, period := 3
#	][four == 0, period := 4
#	][, c("period", "return.m", "alpha_capm", "alpha_3", "alpha_4", "alpha_5", "group")]


# a <- a[, period3 := Reduce('+', shift(period2, 0:4, fill = c(1:4)))]

# dt[, desiredOutcome := Reduce('+', shift(condition, 1:4, fill = c(1:4)), by = a]


figure <- data.f[, .(return.m = mean(return.m), alpha_capm = mean(alpha_capm), alpha_3 = mean(alpha_3), alpha_4 = mean(alpha_4), alpha_5 = mean(alpha_5)), keyby = .(period, group)]

save(figure, file = "figure.RData")

# 是否6期还是12期
medium <- figure[group == 2, .SD]
high <- figure[group == 3, .SD]
low <- figure[group == 1, .SD]

high = melt(high, id.vars = c("period", "group"), measure.var = c("return.m", "alpha_capm", "alpha_3", "alpha_4", "alpha_5"))

g.high <- ggplot(high[variable == 'alpha_3' | variable == 'alpha_4' | variable == 'alpha_5'], aes(x = period)) +
	geom_line(aes(y = value, linetype = variable)) +
	scale_y_continuous(limits = c(-8 * 10 ^ (-4), 2 * 10 ^ (-4)), expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(y = 'performance', title = '冒险程度增加最多组') +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank(),
	   axis.title.x = element_text(family = "RMN", face = "italic", size = 12),
	   axis.title.y = element_text(family = "RMN", face = "italic", size = 12),
	   axis.line = element_line(linetype = 1),
	   plot.title = element_text(hjust = 0.5),
	   legend.text = element_text(size = 8),
	   legend.title = element_blank(),
	   legend.position = c(0.85, 0.15),
)

ggsave('high.jpg', g.high, width = 5, height = 4)

low = melt(low, id.vars = c("period", "group"), measure.var = c("return.m", "alpha_capm", "alpha_3", "alpha_4", "alpha_5"))

g.low <- ggplot(low[variable == 'alpha_3' | variable == 'alpha_4' |variable == 'alpha_5'], aes(x = period)) +
	geom_line(aes(y = value, linetype = variable)) +
	scale_y_continuous(limits = c(-8 * 10 ^ (-4), 2 * 10 ^ (-4)),
	expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(y = 'performance', title = '冒险程度减少最多组') +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank(),
	   axis.title.x = element_text(family = "RMN", face = "italic", size = 12),
	   axis.title.y = element_text(family = "RMN", face = "italic", size = 12),
	   axis.line = element_line(linetype = 1),
	   plot.title = element_text(hjust = 0.5),
	   legend.text = element_text(size = 8),
	   legend.title = element_blank(),
	   legend.position = c(0.85, 0.15),
)

ggsave('low.jpg', g.low, width = 5, height = 4)


medium = melt(medium, id.vars = c("period", "group"), measure.var = c("return.m", "alpha_capm", "alpha_3", "alpha_4", "alpha_5"))

g.medium <- ggplot(medium[variable == 'alpha_3' | variable == 'alpha_4' |variable == 'alpha_5'], aes(x = period)) +
	geom_line(aes(y = value, linetype = variable)) +
	scale_y_continuous(limits = c(-8 * 10 ^ (-4), 2 * 10 ^ (-4)), expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(y = 'performance', title = '冒险程度变化最少组') +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank(),
	   axis.title.x = element_text(family = "RMN", face = "italic", size = 12),
	   axis.title.y = element_text(family = "RMN", face = "italic", size = 12),
	   axis.line = element_line(linetype = 1),
	   plot.title = element_text(hjust = 0.5),
	   legend.text = element_text(size = 8),
	   legend.title = element_blank(),
	   legend.position = c(0.85, 0.15),
)

ggsave('medium.jpg', g.medium, width = 5, height = 4)