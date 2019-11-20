# 到底是不是Buy
load('IOquarter.RData')

IO <- IO[, .(stock.id, date, IOS, IOF, IOI, IOQ, sum)]

IO <- IO[, lapply(colnames(IO[, 3:7]), str_c, ".shift") %>% unlist() := lapply(.SD[, 2:6], shift, n = 1, fill = NA, type = 'lag'), keyby = .(stock.id)
	][, IOS.buy := ifelse(IOS > IOS.shift, 1, 0)
	][, IOF.buy := ifelse(IOF > IOF.shift, 1, 0)
	][, IOI.buy := ifelse(IOI > IOI.shift, 1, 0)
	][, IOQ.buy := ifelse(IOQ > IOQ.shift, 1, 0)
	][, sum.buy := ifelse(sum > sum.shift, 1, 0)]

IO <- IO[, .(stock.id, date, IOS, IOF, IOI, IOQ, sum, IOS.buy, IOF.buy, IOI.buy, IOQ.buy, sum.buy)
	][, year := year(date)
	][, quarter := quarter(date)]

# 加入yearly数据
load('uel2.RData')
uel <- uel[, c("stkcd", "qualified", "Markettype") := NULL
	][, - (3:16)]

IO <- uel[IO, on = .(stock.id, year)]

# add uel quarterly
uel.m <- fread('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/uel_month1.csv')
uel.m <- uel.m[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(UEL= mean(UEL, na.rm = TRUE)), keyby = .(stock.id, year, quarter)]

# match
IO <- uel.m[IO, on = .(stock.id, year, quarter)]

save(IO, file = 'IOquarterdeal.RData')

write.csv(IO, "C://Users//shenfan//Desktop//IO.csv")

