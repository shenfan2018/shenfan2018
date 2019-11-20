## fund TK
fund.TK <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/fund-returnwithTK20190328.xlsx")
fund.TK <- as.data.table(fund.TK)
# 所有char变成numeric
fund.TK <- fund.TK[, colnames(fund.TK[, 7:18]) := lapply(.SD[, 7:18], as.numeric)
	][, date := as.Date(date)]

# id的问题
fund.TK <- fund.TK[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")
	][, 1:10]

setnames(fund.TK, 7:10, c("fund.TK", "fund.PW", "fund.LA", "fund.CC"))

fund.TK <- fund.TK[!is.nan(fund.TK)]

## portfolio TK
load("addholdret.RData")
SJ <- SJ[, .(id, year, month, TK, PW, LA, CC)]

data <- SJ[fund.TK, on = .(id, year, month), nomatch = 0]

data <- data[, TK.gap := fund.TK - TK
	][, PW.gap := fund.PW - PW
	][, LA.gap := fund.LA - LA
	][, CC.gap := fund.CC - CC]

data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK.gap, 5), keyby = .(date)
	][, PW.g := ntile(PW.gap, 5), keyby = .(date)
	][, LA.g := ntile(LA.gap, 5), keyby = .(date)
	][, CC.g := ntile(TK.gap, 5), keyby = .(date)]


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
