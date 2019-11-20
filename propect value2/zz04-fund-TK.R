fund.TK <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/fund-returnwithTK20190328.xlsx")
fund.TK <- as.data.table(fund.TK)
# 所有char变成numeric
fund.TK <- fund.TK[, colnames(fund.TK[, 7:18]) := lapply(.SD[, 7:18], as.numeric)
	][, date := as.Date(date)]

# id的问题
fund.TK <- fund.TK[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")]

##################################################
# 先做TK
data <- fund.TK[!is.nan(TK)
	][, .(id, date, year, month, month_return, TK, PW, LA, CC)]
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, PW.g := ntile(PW, 5), keyby = .(date)
	][, LA.g := ntile(LA, 5), keyby = .(date)
	][, CC.g := ntile(TK, 5), keyby = .(date)]

############################# TK
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, CC.g)
	][!is.na(ret)]

low <- TK.g[CC.g == 1]
high <- TK.g[CC.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[CC.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


########################################################
# 先做TK48
data <- fund.TK[!is.nan(TK48)
	][, .(id, date, year, month, month_return, TK48, PW48, LA48, CC48)]
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK48, 5), keyby = .(date)
	][, PW.g := ntile(PW48, 5), keyby = .(date)
	][, LA.g := ntile(LA48, 5), keyby = .(date)
	][, CC.g := ntile(TK48, 5), keyby = .(date)]

############################# TK48
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, CC.g)
	][!is.na(ret)]

low <- TK.g[CC.g == 1]
high <- TK.g[CC.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[CC.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

########################################################
# 先做TK36
data <- fund.TK[!is.nan(TK36)
	][, .(id, date, year, month, month_return, TK36, PW36, LA36, CC36)]
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 2, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK36, 5), keyby = .(date)
	][, PW.g := ntile(PW36, 5), keyby = .(date)
	][, LA.g := ntile(LA36, 5), keyby = .(date)
	][, CC.g := ntile(TK36, 5), keyby = .(date)]

############################# TK48
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[TK.g == 3, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)



##########################################################################################################################################################double sorting
fund.TK <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/fund-returnwithTK20190328.xlsx")
fund.TK <- as.data.table(fund.TK)
# 所有char变成numeric
fund.TK <- fund.TK[, colnames(fund.TK[, 7:18]) := lapply(.SD[, 7:18], as.numeric)
	][, date := as.Date(date)]

# id的问题
fund.TK <- fund.TK[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")]

##############################
# 先做TK
data <- fund.TK[!is.nan(TK)
	][, .(id, date, year, month, month_return, TK, PW, LA, CC)]
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, month_return.g := ntile(month_return, 5), keyby = .(date)
	][, TK.g := ntile(TK, 5), keyby = .(date, month_return.g)
	][, PW.g := ntile(PW, 5), keyby = .(date, month_return.g)
	][, LA.g := ntile(LA, 5), keyby = .(date, month_return.g)
	][, CC.g := ntile(TK, 5), keyby = .(date, month_return.g)]

############################# TK
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g, month_return.g)
	][!is.na(ret)
	][date > as.Date("2009-03-31")]


low <- TK.g[TK.g == 1 & month_return.g == 5]
high <- TK.g[TK.g == 5 & month_return.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[TK.g == 5 & month_return.g == 1, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)



data[, felm(month_return.1 ~ TK + month_return | id + year)] %>% summary()