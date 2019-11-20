#################################################################### portfolio用T+1 期
load("fund-portfolio-q.RData")

pf <- portfolio.q
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, name, year, quarter, stock.id, MarketValue)]

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.NAV <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter, month)]

mutual <- portfolio.q[order(id)
	][, .(id)
	][, .(id = unique(id))]
mutual <- mutual[["id"]]

load("PV.RData")
date <- propect[!is.na(TK)
	][, .(date = unique(date))
	][, date := as.Date(date)
	][date > as.Date("2003-12-31")]
date <- date[["date"]]

stock <- portfolio.q[order(stock.id)
	][, .(stock.id)
	][, .(stock.id = unique(stock.id))]
stock <- stock[["stock.id"]]

propect <- propect[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)]

s.d <- CJ(stock.id = stock, date = date)
s.d <- s.d[, year := year(date)
	][, quarter := quarter(date)]

# 这个版本的stock proportion未向后一期 一期为半年
# data <- list()
# for (i in 1:693) {
#	fund1 <- mutual[i]
#	data[[i]] <- s.d[, id := fund1]
#	data[[i]] <- pf[s.d, on = .(stock.id, id, year, sem), nomatch = 0]
#	data[[i]] <- pv[data[[i]], on = .(stock.id, date), nomatch = 0
#	][!is.na(TK)
#	][, stock.p := MarketValue / sum(MarketValue, na.rm = TRUE), keyby = .(date)
#	][, .(ztk = sum(stock.p * TK)), keyby = .(date)
#	][, id := fund1]

#	rbindlist(data)
#}

#roll <- data.table(i = 1:693, data)
#roll <- roll[1:693]
#syf <- roll[, rbindlist(.SD[['data']]), by = i]


# 这个版本的stock proportion向后一期 一期为一个季度
data <- list()
for (i in 1:693) {
	fund1 <- mutual[i]
	data[[i]] <- s.d[, id := fund1]
	# 这里滞后
	data[[i]] <- pf[s.d, on = .(stock.id, id, year, quarter), nomatch = 0
	][, year := ifelse(quarter==4, year + 1, year)
	][, quarter := ifelse(quarter == 4, 1, quarter + 1)
	][, .(id, year, quarter, stock.id, MarketValue)]
	data[[i]] <- propect[data[[i]], on = .(stock.id, year, quarter), nomatch = 0
	][!is.na(TK)
	][, stock.p := MarketValue / sum(MarketValue, na.rm = TRUE), keyby = .(date)
	][, .(TK = sum(stock.p * TK), PW = sum(stock.p * PW), LA = sum(stock.p * LA), CC = sum(stock.p * CC)), keyby = .(date)
	][, id := fund1]

	rbindlist(data)
}

roll <- data.table(i = 1:693, data)
roll <- roll[1:693]
syf <- roll[, rbindlist(.SD[['data']]), by = i]

syf <- syf[, year := year(date)
	][, month := month(date)
	][, .(id, year, month, date, TK, PW, LA, CC)]

fund.PV.q <- syf

save(fund.PV.q, file = "fundPV-q.RData")


####################################################################portfolio 用T期
load("fund-portfolio-q.RData")

pf <- portfolio.q
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, name, year, quarter, stock.id, MarketValue)]

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.NAV <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter, month)]

mutual <- portfolio.q[order(id)
	][, .(id)
	][, .(id = unique(id))]
mutual <- mutual[["id"]]

load("PV.RData")
date <- propect[!is.na(TK)
	][, .(date = unique(date))
	][, date := as.Date(date)
	][date > as.Date("2003-12-31")]
date <- date[["date"]]

stock <- portfolio.q[order(stock.id)
	][, .(stock.id)
	][, .(stock.id = unique(stock.id))]
stock <- stock[["stock.id"]]

propect <- propect[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)]

s.d <- CJ(stock.id = stock, date = date)
s.d <- s.d[, year := year(date)
	][, quarter := quarter(date)]

 #这个版本的stock proportion未向后一期 一期为一个季度
 data <- list()
 for (i in 1:693) {
	fund1 <- mutual[i]
	data[[i]] <- s.d[, id := fund1]
	data[[i]] <- pf[s.d, on = .(stock.id, id, year, quarter), nomatch = 0]
	data[[i]] <- propect[data[[i]], on = .(stock.id, date), nomatch = 0
	][!is.na(TK)
	][, stock.p := MarketValue / sum(MarketValue, na.rm = TRUE), keyby = .(date)
	][, .(TK = sum(stock.p * TK), PW = sum(stock.p * PW), LA = sum(stock.p * LA), CC = sum(stock.p * CC)), keyby = .(date)
	][, id := fund1]

	rbindlist(data)
}

roll <- data.table(i = 1:693, data)
roll <- roll[1:693]
syf <- roll[, rbindlist(.SD[['data']]), by = i]


# 这个版本的stock proportion向后一期 一期为一个季度
#data <- list()
#for (i in 1:693) {
#	fund1 <- mutual[i]
#	data[[i]] <- s.d[, id := fund1]
	# 这里滞后
#	data[[i]] <- pf[s.d, on = .(stock.id, id, year, quarter), nomatch = 0
#	][, year := ifelse(quarter == 4, year + 1, year)
#	][, quarter := ifelse(quarter == 4, 1, quarter + 1)
#	][, .(id, year, quarter, stock.id, MarketValue)]
#	data[[i]] <- propect[data[[i]], on = .(stock.id, year, quarter), nomatch = 0
#	][!is.na(TK)
#	][, stock.p := MarketValue / sum(MarketValue, na.rm = TRUE), keyby = .(date)
#	][, .(TK = sum(stock.p * TK), PW = sum(stock.p * PW), LA = sum(stock.p * LA), CC = sum(stock.p * CC)), keyby = .(date)
#	][, id := fund1]

#	rbindlist(data)
#}

#roll <- data.table(i = 1:693, data)
#roll <- roll[1:693]
#syf <- roll[, rbindlist(.SD[['data']]), by = i]

syf <- syf[, year := year(date)
	][, month := month(date)
	][, .(id, year, month, date, TK, PW, LA, CC)]

fund.PV.q <- syf

save(fund.PV.q, file = "fundPV-qt.RData")