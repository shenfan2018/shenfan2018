load("fundPV-qt.RData")
data <- fund.PV.q

# 加入monthly return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter, month)]

data <- ret[fund.PV.q, on = .(id, year, month), nomatch = 0]


#################################################################### single sorting
## t期PV with t+1期return 一期为month
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, PW.g := ntile(PW, 5), keyby = .(date)
	][, LA.g := ntile(LA, 5), keyby = .(date)
	][, CC.g := ntile(TK, 5), keyby = .(date)]
 

################################### TK
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date,TK.g)
	][!is.na(ret)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[TK.g == 5, ret])
t.test(dif[, dif])


fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

##########################################################
#非mean
nomean <- data
t.test(nomean[CC.g == 5, month_return.1])
t.test(nomean[CC.g == 5, month_return.1], nomean[CC.g == 1, month_return.1])


######################################################################################################################################semi-annual 
load("fundPV-st.RData")
data <- fund.PV

# 加入monthly return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)]

data <- ret[fund.PV, on = .(id, year, month), nomatch = 0]


#################################################################### single sorting
## t期PV with t+1期return 一期为month
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, PW.g := ntile(PW, 5), keyby = .(date)
	][, LA.g := ntile(LA, 5), keyby = .(date)
	][, CC.g := ntile(TK, 5), keyby = .(date)]


################################### TK
TK.g <- data[, .(ret = mean(month_return)), keyby = .(date, CC.g)
	][!is.na(ret)]

low <- TK.g[CC.g == 1]
high <- TK.g[CC.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[CC.g == 5, ret]) 

t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

##########################################################
#非mean total average
nomean <- data[!is.na(month_return)]
t.test(nomean[CC.g == 5, month_return])
t.test(nomean[CC.g == 5, month_return], nomean[CC.g == 1, month_return])
