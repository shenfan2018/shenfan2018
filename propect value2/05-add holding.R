## 数据集叫SJ
load("fundPV-st1.RData")
SJ <- fund.PV

# 加入monthly return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, sem, month)]

SJ <- ret[fund.PV, on = .(id, year, month)]

## 加入Holding return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, name, year, sem, stock.id, MarketValue)]

mutual <- portfolio[order(id)
	][, .(id)
	][, .(id = unique(id))]
mutual <- mutual[["id"]]

load("PV.RData")
date <- propect[!is.na(TK)
	][, .(date = unique(date))
	][, date := as.Date(date)
	][date > as.Date("2003-12-31")]
date <- date[["date"]]

stock <- portfolio[order(stock.id)
	][, .(stock.id)
	][, .(stock.id = unique(stock.id))]
stock <- stock[["stock.id"]]

s.d <- CJ(stock.id = stock, date = date)
s.d <- s.d[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# 所有stock的monthly return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/stock.RData")
#整理
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, date := as.Date(date)]
a <- stock
a <- a[order(stock.id, date)
	][, year := year(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, .(stock.mr = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]
# 加入时间
date <- read_excel("C:/Users/shenfan/Desktop/prospect value/date.xlsx")
date <- as.data.table(date)
date <- date[, year := year(date)
	][, month := month(date)]
a <- date[a, on = .(year, month)]

# 这个版本的stock proportion向后一期 一期为半年
data <- list()
for (i in 1:693) {
	fund1 <- mutual[i]
	data[[i]] <- s.d[, id := fund1]
	# 这里滞后
	data[[i]] <- pf[s.d, on = .(stock.id, id, year, sem), nomatch = 0
	][, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)
	][, .(id, year, sem, stock.id, MarketValue)]
	data[[i]] <- a[data[[i]], on = .(stock.id, year, sem), nomatch = 0
	][, stock.p := MarketValue / sum(MarketValue, na.rm = TRUE), keyby = .(date)
	][, .(holding.ret = sum(stock.p * stock.mr)), keyby = .(date)
	][, id := fund1]

	rbindlist(data)
}

roll <- data.table(i = 1:693, data)
roll <- roll[1:693]
syf <- roll[, rbindlist(.SD[['data']]), by = i]

syf <- syf[, year := year(date)
	][, month := month(date)
	][, .(id, year, month, date, holding.ret)]

####################################################################
# match
SJ <- syf[SJ, on = .(id, year, month)
	][, i.date := NULL]

save(SJ, file = "addholdret.RData")


