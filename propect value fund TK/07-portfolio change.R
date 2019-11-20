load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, year, sem, stock.id, Shares)]

# 所有基金
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
fund <- portfolio[order(id)
	][, .(id)
	][, .(id = unique(id))]
fund <- fund[["id"]]

date <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, month := month(date)
	][month == 6 | month == 12
	][date < as.Date("2018-01-01")
	][, .(date)]
date <- date[["date"]]

# 所有stock
A <- read_excel("C:/Users/shenfan/Desktop/Buffett alpha/A股.xlsx")
A <- as.data.table(A)
setnames(A, 1:2, c("code", "name"))
A <- A[, stock.id := substring(code, 1, 6)
	][, .(stock.id)]
A <- A[["stock.id"]]

#perfect 三列相拼
data <- CJ(id = fund, stock.id = A, date = date)

data <- data[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# pfonly 每个基金买过的股票，选出来
pfonly <- pf[order(id, stock.id)
	][, .(stock.id = unique(stock.id)), keyby = .(id)]

data <- pfonly[data, on = .(id, stock.id), nomatch = 0]

# 匹配真实的
data <- pf[data, on = .(id, stock.id, year, sem)]


# 现在做的都是当期，也就是当期加仓还是当期减仓
# 购买：前期没有买过
syf <- data[, Shares.1 := shift(Shares, n = 1, fill = NA, type = "lag"), keyby = .(id, stock.id)
	][, buy1 := ifelse(is.na(Shares.1) & !is.na(Shares), 1, 0)]

# 加仓： 现在比之前买的多（前期是两期都买的）
syf <- syf[, buy2 := ifelse( Shares > Shares.1, 1, 0)]

# 卖出：也就是下一期就没有了
syf <- syf[, sell1 := ifelse(is.na(Shares) & !is.na(Shares.1), 1, 0)]

# 减仓：现在比之前的少
syf <- syf[, sell2 := ifelse(Shares < Shares.1, 1, 0)]

# 整理
syf <- syf[, buy := ifelse(!is.na(buy2), buy2, buy1)
	][, sell := ifelse(!is.na(sell2), sell2, sell1)
	][, .(id, stock.id, date, year, sem, Shares, buy, sell)
	][buy == 1 | sell == 1]


# high TK 的基金
fund.TK <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/fund-returnwithTK20190328.xlsx")
fund.TK <- as.data.table(fund.TK)
# 所有char变成numeric
fund.TK <- fund.TK[, colnames(fund.TK[, 7:18]) := lapply(.SD[, 7:18], as.numeric)
	][, date := as.Date(date)]

# id的问题
fund.TK <- fund.TK[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")]

fund.TK <- fund.TK[, - (11:18)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# sem 这里选择了10%
fund.TK <- fund.TK[!is.nan(TK)
	][, .(TK = mean(TK)), keyby = .(id, year, sem)
	][, TK.g := ntile(TK, 5), keyby = .(year, sem)
	][TK.g == 5]

highTKF <- fund.TK[syf, on = .(id, year, sem), nomatch = 0]

############################################################
# 所有buy的stock
buy.stock <- highTKF[buy == 1
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]

# 所有sell的stock
sell.stock <- highTKF[sell == 1
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]



# buy
buy.stock <- highTKF[buy == 1
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]

load("stocks.RData")
stock.s <- stock.s[, MV := log(MV)]
buy.stock <- buy.stock[stock.s, on = .(stock.id, year, sem), nomatch = 0
	][order(year, sem, stock.id)
	][, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE)), keyby = .(year, sem)]

buy <- buy.stock[, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE))]

# sell
sell.stock <- highTKF[sell == 1
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]

sell.stock <- sell.stock[stock.s, on = .(stock.id, year, sem), nomatch = 0
	][order(year, sem, stock.id)
	][, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE)), keyby = .(year, sem)]

sell <- sell.stock[, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE))]

# dif
dif <- sell.stock[buy.stock, on = .(year, sem)
	][, .(MV.d = i.MV - MV, BM.d = i.BM - BM, GP.d = i.GP - GP, stockTK.d = i.stockTK - stockTK, max.d = i.max - max, skewness.d = i.skewness - skewness, semi_ret.d = i.semi_ret - semi_ret)]

t.test(dif[, stockTK.d])

fit <- lm(stockTK.d ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

