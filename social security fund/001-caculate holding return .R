### 新版本 single social holding return sem-annual
load("socialpf.RData")
social <- social[, proportion := MV.current / sum(MV.current, na.rm = TRUE), keyby = .(fund, year, sem)]

# stock ,这里滞后了，若要同期，则删除最后两行
load("stock.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_ret = prod(Dretwd2, na.rm = TRUE) - 1), keyby = .(stock.id, year, sem, month)]

# 往后滞后了一期，social的
social <- social[, .(fund, stock.id, date, year, sem, proportion)
	][, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]

# 目前ret是 期的持仓算t期的收益
social.ret <- stock.m[social, on = .(stock.id, year, sem)
	][, .(ret.s = sum(proportion * month_ret, na.rm = TRUE)), keyby = .(fund, year, sem, month)
	][year < 2018
	][!is.na(month)]

save(social.ret, file = "social_ret.RData")

#####################################################################
# mutual fund holding semi-annual
# get holding return
load("portfolio.RData")
portfolio <- portfolio[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)]

# stock ,这里滞后了，若要同期，则删除最后两行
load("stock.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_ret = prod(Dretwd2, na.rm = TRUE) - 1), keyby = .(stock.id, year, sem, month)]

# 往后滞后了一期，mutual fund的
portfolio <- portfolio[, .(id, stock.id, date, year, sem, proportion.stock)
	][, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]

# match 牛
# portfolio <- date[portfolio, on = .(year, sem), allow.cartesian = TRUE]
# try by=.EACHI to run j for each group to avoid the large allocation. If you are sure you wish to proceed, rerun with allow.cartesian=TRUE. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and data.table issue tracker for advice.

# 目前ret是 期的持仓算t期的收益
fund.ret <- stock.m[portfolio, on = .(stock.id, year, sem), allow.cartesian = TRUE
	][, .(ret.f = sum(proportion.stock * month_ret, na.rm = TRUE)), keyby = .(id, year, sem, month)
	][year < 2018
	][!is.na(month)]

######### add real return
load("fund-NAV.RData")
data.NAV <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(ret.f.raw = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]
# match
fund.ret <- data.NAV[fund.ret, on = .(id, year, sem, month)
	][, .(id, year, sem, month, ret.f, ret.f.raw)]

save(fund.ret, file = "fund_ret.RData")

# 原版51486


fund.ret <- fund.ret[data.NAV, on = .(id, year, sem, month)
	][, .(id, year, sem, month, ret.f, ret.f.raw)]

load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/fundchar.RData")
fund.char <- fund.char[, .SD[1], keyby = .(id)
	][, .(id, name)]
# match
fund.ret <- fund.char[fund.ret, on = .(id), nomatch = 0]

save(fund.ret, file = "fund_retnew.RData")

# 现在55581

