load("monthlydata.RData")
# 依旧月度
data <- fund.TK[!is.nan(TK)]

data <- data[, TK.g := ntile(TK, 5), keyby = .(year, month)
	][, .(TK = mean(TK)), keyby = .(year, month, TK.g)
	][, .(TK = mean(TK)), keyby = TK.g]

low <- data[TK.g == 1]
high <- data[TK.g == 5]
dif <- low[high, on = .(year, month)
	][, TK.d := i.TK - TK]

t.test(dif[, TK.d])

fit <- lm(TK.d ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

# 半年
data <- fund.TK[!is.nan(TK)]

data <- data[, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(TK = mean(TK)), keyby = .(id, year, sem)
	][, TK.g := ntile(TK, 5), keyby = .(year, sem)
	][, .(TK = mean(TK)), keyby = .(year, sem, TK.g)
	][, .(TK = mean(TK)), keyby = TK.g]

low <- data[TK.g == 1]
high <- data[TK.g == 5]
dif <- low[high, on = .(year, sem)
	][, TK.d := i.TK - TK]

t.test(dif[, TK.d])

fit <- lm(TK.d ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


#######################################################################
# 选出高TK的fund
data <- fund.TK[!is.nan(TK)]

data <- data[, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(TK = mean(TK)), keyby = .(id, year, sem)
	][, TK.g := ntile(TK, 5), keyby = .(year, sem)]


# portfolio
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, name, year, sem, stock.id, MarketValue)]

pf <- data[pf, on = .(id, year, sem), nomatch = 0]

# 到股票
pf <- pf[, .(stock.id, year, sem, TK.g)
	][order(stock.id, year, sem)]

###########################################################################
# TK==1

# stock.TK <- pf[TK.g == 1
#	][, .(stock.id = unique(stock.id)), keyby = .(year, sem, TK.g)]

# load("stocks.RData")
# stock.s <- stock.s[, MV := log(MV)]
# stock.s <- stock.TK[stock.s, on = .(stock.id, year, sem), nomatch = 0
#	][, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE)), keyby = .(year, sem)
#	][,TK.g :=1]

###########################################################################
data <- list()
for (i in 1:5) {
	stock.TK <- pf[TK.g == i
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem, TK.g)]

	load("stocks.RData")
	stock.s <- stock.s[, MV := log(MV)]
	data[[i]] <- stock.TK[stock.s, on = .(stock.id, year, sem), nomatch = 0
	][, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE)), keyby = .(year, sem)
	][, TK.g := i]

	rbindlist(data)
}

roll <- data.table(i = 1:5, data)
roll <- roll[1:5]
syf <- roll[, rbindlist(.SD[['data']]), by = i]

export <- syf[, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE)), keyby = .(TK.g)]

write.csv(export, "C://Users//shenfan//Desktop//table3.csv")

low <- syf[TK.g == 1]
high <- syf[TK.g == 5]
dif <- low[high, on = .(year, sem)
	][, .(MV.d = i.MV - MV, BM.d = i.BM - BM, GP.d = i.GP - GP, stockTK.d = i.stockTK - stockTK, max.d = i.max - max, skewness.d = i.skewness - skewness, semi_ret.d = i.semi_ret - semi_ret)]

t.test(dif[, semi_ret.d])

fit <- lm(semi_ret.d ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)




