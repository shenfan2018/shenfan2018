# all
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio[, .(id, date, stock.id)
	][, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]

# 滞后，也就是下一期当作持有
pf <- pf[, year := ifelse(sem == 2, year + 1, year)
	][, month := ifelse(sem == 2, 5, 9)
	][, buy := 1]

########################################################################
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

# sem 这里选择了20%
fund.TK <- fund.TK[!is.nan(TK)
	][, .(TK = mean(TK)), keyby = .(id, year, sem)
	][, TK.g := ntile(TK, 5), keyby = .(year, sem)
	][TK.g == 5]

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio[, .(id, date, stock.id)
	][, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

# match High TK
pf <- fund.TK[pf, on = .(year, sem, id), nomatch = 0
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]

# 滞后，也就是下一期当作持有
pf <- pf[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)
	][, buy := 1]

########################################################################
# 变成monthly的数据
load("C:/Users/shenfan/source/repos/shenfan2018/Buffett alpha/stock20030101-20190331.RData")
stock.m <- stock
stock.m <- stock.m[order(stock.id, date)
	][, year := year(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_ret = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

# 这里改一下
# match 变成data，主表
# data <- pf[stock.m, on = .(stock.id, year, sem)
#	][, buy := ifelse(is.na(buy), 0, 1)]

data <- pf[stock.m, on = .(stock.id, year, month)
	][, buy := ifelse(is.na(buy), 0, 1)]

# stock TK monthly 
load("C:/Users/shenfan/source/repos/shenfan2018/propect value2/PV.RData")
propect <- propect[, year := year(date)
	][, month := month(date)
	][, .(stock.id, year, month, TK)]

# match
data <- propect[data, on = .(stock.id, year, month), nomatch = 0]

# 先试验5月份的版本
data <- data[, year2 := shift(year - 1, n = 4, fill = NA, type = "lag"), keyby = .(stock.id)]

# 孙老师
sun <- fread("C:/Users/shenfan/Desktop/prospect value/firm_chara.csv")
sun <- sun[, .(stkcd, year, size, bm, INV, GP, ROE, mom)
	][, stkcd := as.character(stkcd)
	][, stkcd := str_pad(stkcd, 6, side = "left", pad = "0")]
setnames(sun, c("stkcd", "year"), c("stock.id", "year2"))

# BM SIZE GP
# load("stocks.RData")

# stock.s <- stock.s[, .(MV = unique(MV), GP = unique(GP), BM = unique(BM)), keyby = .(stock.id, year)]
# setnames(stock.s, "year", "year2")

data <- sun[data, on = .(stock.id, year2)]

# date
date <- read_excel("C:/Users/shenfan/Desktop/prospect value/date.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)]

data <- date[data, on = .(year, month)]

# clean
# 先是win
a1 <- quantile(data[, month_ret], 0.99, na.rm = TRUE)
a2 <- quantile(data[, month_ret], 0.01, na.rm = TRUE)
data <- data[order(month_ret)
	][, extremum := ifelse(month_ret > a1, 2, ifelse(month_ret < a2, 1, 0))
	][extremum == 1, month_ret := month_ret[.N]
	][extremum == 2, month_ret := month_ret[1]]

# four
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, .(year, month, mkt, rf)]

data <- mkt[data, on = .(year, month)]

data <- data[order(stock.id, date)
	][, ret_rf := month_ret - rf
	][, TK.1 := shift(TK, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, ret_rf.1 := shift(ret_rf, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)]

# buy 也变成五月份的版本
# data <- data[, buy.5 := shift(buy, n = 4, fill = NA, type = "lag"), keyby = .(stock.id)]


write.csv(data, "C://Users//shenfan//Desktop//mydatam4.csv")



pmg(month_return.1 ~ TK, data, index = c("date", "stock.id")) %>% summary()


pmg(month_return.1 ~  bm, data, index = c("date", "stock.id")) %>% summary()

write.csv(data, "C://Users//shenfan//Desktop//mydatam.csv")




