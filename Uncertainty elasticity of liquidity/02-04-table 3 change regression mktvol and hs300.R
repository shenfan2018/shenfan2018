load('IOquarter.RData')
IO <- IO[, .(stock.id, date, IOS, IOF, IOI, IOQ, sum)]

# difference
IO <- IO[order(stock.id, date)
	][, lapply(colnames(IO[, 3:7]), str_c, ".d") %>% unlist() := lapply(.SD[, 2:6], diff, difference = 1), keyby = .(stock.id)
	][, lapply(colnames(IO[, 3:7]), str_c, ".d") %>% unlist() := lapply(.SD[, 7:11], shift, n = 1, type = 'lag', fill = NA), keyby = .(stock.id)
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, date, year, quarter, IOS.d, IOF.d, IOI.d, IOQ.d, sum.d)]

# market voliatility （daily?） top 10%
factor <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_daily/three_four_five_factor_daily/fivefactor_daily.csv")
factor <- factor[, trddy := as.Date(trddy)
	][, year := year(trddy)
	][, quarter := quarter(trddy)
	][, mkt := mkt_rf + rf
	][, .(vol = sd(mkt)), keyby = .(year, quarter)
	][year < 2018 & year > 2003
	][, vol.g := ntile(vol, 5)
	][, mkt.vol := ifelse(vol.g == 5, 1, 0)]

# match
data <- factor[IO, on = .(year, quarter), nomatch = 0]

# match size
# annual uel
uel <- fread("C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/uel.csv")
uel <- uel[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year := year - 1
	][, logbm := log(bm)
	][, logsize := log(size)
	][, .(stock.id, year, logsize, logbm, GP, INV, daily_vola)]

data <- uel[data, on = .(stock.id, year)]

# match price
price <- fread('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/prc.csv')
price <- price[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year := year - 1
	][, .(stock.id, year, prc)]

data <- price[data, on = .(stock.id, year)]

# 是否沪深300
hs300 <- read_excel('C:/Users/shenfan/Desktop/社保/data/沪深300成分股.xlsx')
hs300 <- as.data.table(hs300)
hs300 <- hs300[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, year, quarter)
	][, hs300 := 1]

# match
data <- hs300[data, on = .(stock.id, year, quarter)]

data <- data[, hs300 := ifelse(is.na(hs300), 0, 1)]


# reg mkt vol
liner1 <- plm(IOS.d ~ IOF.d * mkt.vol + logbm + logsize + daily_vola + prc + GP + INV, data, model = "pooling", effect = "twoways", index = c("stock.id", "date"))

liner2 <- plm(IOS.d ~ IOI.d * mkt.vol + logbm + logsize + daily_vola + prc + GP + INV, data, model = "pooling", effect = "twoways", index = c("stock.id", "date")) 

liner3 <- plm(IOS.d ~ IOQ.d * mkt.vol + logbm + logsize + daily_vola + prc + GP + INV, data, model = "pooling", effect = "twoways", index = c("stock.id", "date")) 

liner4 <- plm(IOS.d ~ sum.d * mkt.vol + logbm + logsize + daily_vola + prc + GP + INV, data, model = "pooling", effect = "twoways", index = c("stock.id", "date"))


stargazer(liner1, liner2, liner3, liner4, type = "html", out = "C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/change.doc", add.lines = list(c("stock", "yes", "yes", "yes", "yes"), c("date", "yes", "yes", "yes", "yes")), report = ('vc*t'))


# reg hs300
liner1 <- plm(IOS.d ~ IOF.d * hs300 + logbm + logsize + daily_vola + prc + GP + INV, data, model = "pooling", effect = "twoways", index = c("stock.id", "date"))

liner2 <- plm(IOS.d ~ IOI.d * hs300 + logbm + logsize + daily_vola + prc + GP + INV, data, model = "pooling", effect = "twoways", index = c("stock.id", "date"))

liner3 <- plm(IOS.d ~ IOQ.d * hs300 + logbm + logsize + daily_vola + prc + GP + INV, data, model = "pooling", effect = "twoways", index = c("stock.id", "date"))

liner4 <- plm(IOS.d ~ sum.d * hs300 + logbm + logsize + daily_vola + prc + GP + INV, data, model = "pooling", effect = "twoways", index = c("stock.id", "date"))

stargazer(liner1, liner2, liner3, liner4, type = "html", out = "C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/change_hs300.doc", add.lines = list(c("stock", "yes", "yes", "yes", "yes"), c("date", "yes", "yes", "yes", "yes")), report = ('vc*t'))


