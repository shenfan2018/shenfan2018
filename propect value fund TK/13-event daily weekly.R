# monthly 
load("monthlydata.RData")
fund.TK <- fund.TK[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]
# TKmax的20%
fund.TK <- fund.TK[!is.nan(TK)
	][!is.na(month_return.1)
	][, TK.g := ntile(TK, 5), keyby = .(date)]

# 这里进行到下一期
sample <- fund.TK[, .(id, year, month, TK.g)
	][, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

# daily
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
data.NAV <- data.NAV[, year := year(date)
	][, month := month(date)]

# match
data <- sample[data.NAV, on = .(id, year, month), nomatch = 0]

# mobile
mobile <- data[, mobile := ifelse(date > as.Date("2014-12-31"), 1, 0)
	][, .(ret = mean(AdjustedNAVGrowth)), keyby = .(date, TK.g, mobile)]

# 选择前后多久
# 多少月
mobile <- mobile[date > as.Date("2014-06-30") & date < as.Date("2015-07-01")]

# 5-1
dif <- mobile[, ret.1 := shift(ret, n = 4, fill = NA, type = "lead"), keyby = .(date)
	][, dif := ret.1 - ret
	][!is.na(dif)
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(mobile)]

write.csv(dif, "C://Users//shenfan//Desktop//aaaa.csv")


# future
future <- data[, future := ifelse(date > as.Date("2015-06-30"), 1, 0)
	][, .(ret = mean(AdjustedNAVGrowth)), keyby = .(date, TK.g, future)]

# 选择前后多久
# 多少月
future <- future[date > as.Date("2014-12-31") & date < as.Date("2016-01-01")]

# 5-1
dif <- future[, ret.1 := shift(ret, n = 4, fill = NA, type = "lead"), keyby = .(date)
	][, dif := ret.1 - ret
	][!is.na(dif)
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(future)]

write.csv(dif, "C://Users//shenfan//Desktop//aaaa.csv")


########################################################## weekly
# monthly 
load("monthlydata.RData")
fund.TK <- fund.TK[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]

# TKmax的20%
fund.TK <- fund.TK[!is.nan(TK)
	][!is.na(month_return.1)
	][, TK.g := ntile(TK, 5), keyby = .(date)]

# 这里进行到下一期
sample <- fund.TK[, .(id, year, month, TK.g)
	][, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

# daily
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
data.NAV <- data.NAV[, year := year(date)
	][, week := isoweek(date)
	][, month := month(date)
	][, week.return := prod(AdjustedNAVGrowth + 1) - 1, keyby = .(id, year, week)
	][, .SD[1], keyby = .(id, year, week)
	][, .(id, date, year, month, week, week.return)]

# match
data <- sample[data.NAV, on = .(id, year, month), nomatch = 0]

# mobile
mobile <- data[, mobile := ifelse(date > as.Date("2014-12-31"), 1, 0)
	][, .(ret = mean(week.return)), keyby = .(date, TK.g, mobile)]

# 选择前后多久
# 多少月
mobile <- mobile[date > as.Date("2014-6-30") & date < as.Date("2015-07-01")]

# 5-1
dif <- mobile[, ret.1 := shift(ret, n = 4, fill = NA, type = "lead"), keyby = .(date)
	][, dif := ret.1 - ret
	][!is.na(dif)
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(mobile)]

write.csv(dif, "C://Users//shenfan//Desktop//aaaa.csv")


# future
future <- data[, future := ifelse(date > as.Date("2015-06-30"), 1, 0)
	][, .(ret = mean(week.return)), keyby = .(date, TK.g, future)]

# 选择前后多久
# 多少月
future <- future[date > as.Date("2014-12-31") & date < as.Date("2016-01-01")]

# 5-1
dif <- future[, ret.1 := shift(ret, n = 4, fill = NA, type = "lead"), keyby = .(date)
	][, dif := ret.1 - ret
	][!is.na(dif)
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(future)]

write.csv(dif, "C://Users//shenfan//Desktop//aaaa.csv")