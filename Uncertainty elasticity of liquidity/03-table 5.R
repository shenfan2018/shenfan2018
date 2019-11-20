load('uel.RData')

# 将NA变成0
uel <- uel[, colnames(uel[, 3:6]) := lapply(.SD[, 3:6], function(x) {
	ifelse(is.na(x), 0, x)
})]

# 加入到monthly stock but yearly ret
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund/stocklhb.RData")
stock.m <- stock
stock.m <- stock.m[order(stock.id, date)
	][, Dretwd2 := Dretwd + 1
	][, month := month(date)
	][, year := year(date)
	][, .(month_return = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)
	][, month_return2 := month_return + 1
	][, tag := seq(1:(.N)), keyby = .(stock.id)]

#reg.roll <- list()
#for (i in 12:192) {
	#reg.roll[[i]] <- stock.m[tag >= i - 11 & tag <= i, {
		#I <- prod(month_return2) - 1 
	#},
	#keyby = .(stock.id)]
	#rbindlist(reg.roll)
#}

#roll <- data.table(tag = 1:192, reg.roll)

#reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

#stock.m <- reg.cof[stock.m, on = .(stock.id, tag)]

## 现在的year return
#stock.m <- stock.m[, year_return := shift(V1, n = 12, fill = NA, type = "lead"), keyby = .(stock.id)
	#][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)
	#][, year.match := ifelse(month > 4, year - 1, year - 2)]


stock.m <- stock.m[, year.match := ifelse(month > 4, year - 1, year - 2)]

# 加入uel
setnames(uel, "year", "year.match")

data <- uel

# 试验 ANA difference, 先按照characterstic分，再按照uel分
# match
data <- data[stock.m, on = .(year.match, stock.id)]

# 全部NA
data <- data[!is.na(fund.end)
	][!is.na(port_uel)]

data <- data[, ANA.g := ntile(social.avg, 5), keyby = .(year, month)
	][!is.na(ANA.g)
	][, uel.g := ntile(port_uel, 5), keyby = .(year, month, ANA.g)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month, ANA.g, uel.g)
	][, ret.2 := shift(ret, n = 4, fill = NA, type = "lead")
	][uel.g == 1
	][, dif := ret.2 - ret
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(ANA.g)]


######################################################################
# uel2
load('uel2.RData')

# 加入到monthly stock but yearly ret
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund/stocklhb.RData")
stock.m <- stock
stock.m <- stock.m[order(stock.id, date)
	][, Dretwd2 := Dretwd + 1
	][, month := month(date)
	][, year := year(date)
	][, .(month_return = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)]

stock.m <- stock.m[, year.match := ifelse(month > 4, year - 1, year - 2)]

# 加入uel
setnames(uel, "year", "year.match")

# now begin
data <- uel

# match
data <- data[stock.m, on = .(year.match, stock.id), nomatch = 0]

# year.match > 2003
data <- data[year.match > 2002
	][year.match < 2017]

# 变格式
data <- data[, .(stock.id, year, month, port_uel, month_return, IOI, IOQ, IOF, IOS, sum, prc, size, bm, mom, INV, GP, ROE, daily_vola, trading_days, float_pct, inshd_percentage, analyst_coverage, num_shareholder, num_employee, illiq, port_beta1, port_beta2, port_beta3, port_beta4, port_beta5)]

## 还是分开来做吧
## 先按照characterstic分，再按照uel分 mutual fund
#table = melt(data, id.vars = 1:5, measure.vars = 6:30)

# mutual fund and sum
a <- data[, .(stock.id, year, month, port_uel, month_return, IOF, sum)]

a = melt(a, id.vars = 1:5, measure.vars = 6:7)

a <- a[!is.na(value)
	][!is.na(port_uel)
	][, value.g := ntile(value, 5), keyby = .(variable, year, month)
	][, port_uel.g := ntile(port_uel, 5), keyby = .(variable, year, month, value.g)]

# difference
a <- a[, .(ret = mean(month_return)), keyby = .(variable, year, month, value.g, port_uel.g)
	][, ret2 := shift(ret, n = 4, fill = NA, type = "lead")
	][port_uel.g == 1
	][, dif := ret2 - ret
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], dif = t.test(dif)$estimate, t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, value.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")



# #####另外四个
a <- data[, .(stock.id, year, month, port_uel, month_return, IOI, IOQ, IOS)]

a = melt(a, id.vars = 1:5, measure.vars = 6:8)

a <- a[!is.na(value)
	][!is.na(port_uel)
	][, value.g := ntile(value, 5), keyby = .(variable, year, month)
	][, value.g := ifelse(value.g == 5, 5, 1)
	][, port_uel.g := ntile(port_uel, 5), keyby = .(variable, year, month, value.g)]

# difference
a <- a[, .(ret = mean(month_return)), keyby = .(variable, year, month, value.g, port_uel.g)
	][, ret2 := shift(ret, n = 4, fill = NA, type = "lead")
	][port_uel.g == 1
	][, dif := ret2 - ret
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], dif = t.test(dif)$estimate, t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, value.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

