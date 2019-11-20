load("week-fund-PV26.RData")

# 加入monthly return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter, month)]

##
data <- ret[PV26, on = .(id, year, month), nomatch = 0]

data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(CC26, 5), keyby = .(date)]

TK.ret <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

low <- TK.ret[TK.g == 1]
high <- TK.ret[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.ret[TK.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


########################################################################### holding return
load("week-fund-PV26.RData")

# 加入holding return
load("holdingret.RData")

##
data <- holdingret[PV26, on = .(id, year, month, date), nomatch = 0]

data <- data[order(id, date)
	][, month_return.1 := shift(holding.ret, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(CC26, 5), keyby = .(date)]

TK.ret <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

low <- TK.ret[TK.g == 1]
high <- TK.ret[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.ret[TK.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)