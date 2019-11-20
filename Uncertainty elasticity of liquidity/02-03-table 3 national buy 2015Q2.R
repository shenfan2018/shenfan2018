national <- read_excel("C:/Users/shenfan/Desktop/社保/data/国家队重仓流通股.xlsx")
national <- as.data.table(national)
setnames(national, 1:14, c("code", "stock.name", "national.name", "date", "realtime", "hold.T", "changes", "hold.T-1", "mv.T", "mv.change", "mv.T-1", 'pro.T', 'pro.change', 'pro.ST')) # pro.ST 上期

# 2015 二季度到三季度
national <- national[, date := as.Date(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, date, national.name, pro.T, pro.change, pro.ST)]

# sum得
a <- national[, .(pro = sum(pro.T)), keyby = .(stock.id, date)
	][date > as.Date('2015-03-31') & date < as.Date('2017-09-30')]

# 扩
date <- a[order(date)
	][, .(date = unique(date))]
date <- date[["date"]]

stockid <- a[order(stock.id)
	][, .(stock.id = unique(stock.id))]
stockid <- stockid[["stock.id"]]

IO.national <- CJ(stock.id = stockid, date = date)

# match
IO.national <- a[IO.national, on = .(stock.id, date)]

# 处理
IO.national <- IO.national[, pro.2 := shift(pro, n = 1, fill = NA, type = 'lead'), keyby = .(stock.id)
	#	][, national.buy := ifelse(is.na(pro.2) & !is.na(pro), 1, 0)  # sell
	][, national.buy := ifelse(is.na(pro) & !is.na(pro.2), 1, 0) # buy
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, date, year, quarter, national.buy)]

################################################## add 02-table3 add pre post
# quarter uel and buy
# add uel quarterly
uel.m <- fread('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/uel_month1.csv')
uel.m <- uel.m[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(uel.m = mean(UEL)), keyby = .(stock.id, year, quarter)
	][, uel.m.2 := shift(uel.m, n = 1, fill = NA, type = 'lead'), keyby = .(stock.id)
	][, uel.dif := uel.m.2 - uel.m]

###########
# whether buy or not
load('IOquarter.RData')

IO <- IO[, .(stock.id, date, IOS, IOF, IOI, IOQ, sum)]
## add
IO <- IO[, lapply(colnames(IO[, 3:7]), str_c, ".shift") %>% unlist() := lapply(.SD[, 2:6], shift, n = 1, fill = NA, type = 'lead'), keyby = .(stock.id)
	][, IOS.buy := ifelse(IOS < IOS.shift, 1, 0)
	][, IOF.buy := ifelse(IOF < IOF.shift, 1, 0)
	][, IOI.buy := ifelse(IOI < IOI.shift, 1, 0)
	][, IOQ.buy := ifelse(IOQ < IOQ.shift, 1, 0)
	][, sum.buy := ifelse(sum < sum.shift, 1, 0)]

IO <- IO[, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, year, quarter, IOS.buy, IOF.buy, IOI.buy, IOQ.buy, sum.buy)]

# match national
IO <- IO.national[IO, on = .(stock.id, year, quarter)]

# 仅选择2015.2到2017.2
IO <- IO[!is.na(date)
	][year == 2015
	][quarter == 2]

# match
data <- uel.m[IO, on = .(stock.id, year, quarter), nomatch = 0]

write.csv(data, "C://Users//shenfan//Desktop//mydatam.csv")
