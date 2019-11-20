load("fundPV-st1.RData")

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

data <- ret[fund.PV, on = .(id, year, month)
	][, quarter := quarter(date)
	][, .(id, date, year, month, month_return, TK, PW, LA, CC)]

write.csv(data, "C://Users//shenfan//Desktop//monthPV.csv")

##############################################################################
fund.TK <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/fund-returnwithTK20190328.xlsx")
fund.TK <- as.data.table(fund.TK)
# 所有char变成numeric
fund.TK <- fund.TK[, colnames(fund.TK[, 7:18]) := lapply(.SD[, 7:18], as.numeric)
	][, date := as.Date(date)]

# id的问题
fund.TK <- fund.TK[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")]

write.csv(fund.TK, "C://Users//shenfan//Desktop//fundPV.csv")

##############################################################################
load("week-fund-PV52.RData")

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, sem, month)]

data <- ret[PV52, on = .(id, year, month)
	][, quarter := quarter(date)
	][, .(id, date, year, month, month_return, TK52, PW52, LA52, CC52)]

write.csv(data, "C://Users//shenfan//Desktop//weekPV52.csv")


###########################################################################
load("week-fund-PV26.RData")

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, sem, month)]

data <- ret[PV26, on = .(id, year, month)
	][, quarter := quarter(date)
	][, .(id, date, year, month, month_return, TK26, PW26, LA26, CC26)]

write.csv(data, "C://Users//shenfan//Desktop//weekPV26.csv")

#####################################################################
load("decayfundPV-st1.RData")

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter, month)]

data <- ret[fund.PV, on = .(id, year, month), nomatch = 0
	][, .(id, date, year, month, month_return, TKrho0.8, TKrho0.85, TKrho0.9)]

write.csv(data, "C://Users//shenfan//Desktop//decayPV.csv")
