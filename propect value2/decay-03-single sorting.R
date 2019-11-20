load("decayfundPV-st1.RData")
data <- fund.PV

# 加入monthly return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter, month)]

data <- ret[fund.PV, on = .(id, year, month), nomatch = 0]

#################################################################### single sorting
## t期PV with t+1期return 一期为month
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK80.g := ntile(TKrho0.8, 5), keyby = .(date)
	][, TK85.g := ntile(TKrho0.85, 5), keyby = .(date)
	][, TK90.g := ntile(TKrho0.9, 5), keyby = .(date)]


################################### TK
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK90.g)
	][!is.na(ret)]

low <- TK.g[TK90.g == 1]
high <- TK.g[TK90.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[TK90.g == 5, ret])
t.test(dif[, dif])


fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)




