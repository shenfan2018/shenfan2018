# fund TK
fund.TK <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/fund-returnwithTK20190328.xlsx")
fund.TK <- as.data.table(fund.TK)
# 所有char变成numeric
fund.TK <- fund.TK[, colnames(fund.TK[, 7:18]) := lapply(.SD[, 7:18], as.numeric)
	][, date := as.Date(date)]

# id的问题
fund.TK <- fund.TK[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")
	][, 1:10]

# 
data <- fund.TK
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]

data <- data[!is.nan(TK)]

# double sorting
data <- data[, month_return.g := ntile(month_return, 5), keyby = .(date)
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

t.test(TK.g[TK.g == 3 & month_return.g == 3, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


# liner regression
data.reg <- data[date > as.Date("2009-03-31")]
data.reg[, felm(month_return.1 ~ TK + month_return | id + year)] %>% summary()
