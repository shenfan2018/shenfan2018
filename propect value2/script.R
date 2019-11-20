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

data <- data[order(id, year, month)
	][, TK.c := c(NA, diff(TK, difference = 1)), keyby = id
	][, PW.c := c(NA, diff(PW, difference = 1)), keyby = id
	][, LA.c := c(NA, diff(LA, difference = 1)), keyby = id
	][, CC.c := c(NA, diff(CC, difference = 1)), keyby = id]

data <- data[!is.na(TK.c)]

data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK.c, 5), keyby = .(date)
	][, PW.g := ntile(PW.c, 5), keyby = .(date)
	][, LA.g := ntile(LA.c, 5), keyby = .(date)
	][, CC.g := ntile(TK.c, 5), keyby = .(date)]

############################# TK
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[TK.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)