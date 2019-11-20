################# month
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, name, year, sem, stock.id, MarketValue)]

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.NAV <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, sem, month)]

mutual <- portfolio[order(id)
	][, .(id)
	][, .(id = unique(id))]
mutual <- mutual[["id"]]

load("decayPV.RData")
date <- TK[!is.na(TKrho0.9)
	][, .(date = unique(date))
	][, date := as.Date(date)
	][date > as.Date("2003-12-31")]
date <- date[["date"]]

stock <- portfolio[order(stock.id)
	][, .(stock.id)
	][, .(stock.id = unique(stock.id))]
stock <- stock[["stock.id"]]

TK <- TK[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

s.d <- CJ(stock.id = stock, date = date)
s.d <- s.d[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

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
	data[[i]] <- TK[data[[i]], on = .(stock.id, year, sem), nomatch = 0
	][!is.na(TKrho0.8)
	][, stock.p := MarketValue / sum(MarketValue, na.rm = TRUE), keyby = .(date)
	][, .(TKrho0.8 = sum(stock.p * TKrho0.8), TKrho0.85 = sum(stock.p * TKrho0.85), TKrho0.9 = sum(stock.p * TKrho0.9)), keyby = .(date)
	][, id := fund1]

	rbindlist(data)
}


roll <- data.table(i = 1:693, data)
roll <- roll[1:693]
syf <- roll[, rbindlist(.SD[['data']]), by = i]

syf <- syf[, year := year(date)
	][, month := month(date)
	][, .(id, year, month, date, TKrho0.8, TKrho0.85, TKrho0.9)]

fund.PV <- syf

save(fund.PV, file = "decayfundPV-st1.RData")

