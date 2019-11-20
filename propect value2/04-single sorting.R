load("3clean.RData")
data <- clean

## t期PV with t+1期return 一期为month
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, PW.g := ntile(PW, 5), keyby = .(date)
	][, LA.g := ntile(LA, 5), keyby = .(date)
	][, CC.g := ntile(TK, 5), keyby = .(date)]

# 各种因子以及alpha
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

################################### TK
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

TK.g <- TK.g[, year := year(date)
	][, month := month(date)]
TK.g <- four[TK.g, on = .(year, month)
	][, rit := ret - rf]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[TK.g == 1, ret])
t.test(TK.g[TK.g == 5, ret])
t.test(dif[, dif])

sd(TK.g[TK.g == 5, ret])
sd(dif[, dif])

skewness(TK.g[TK.g == 5, ret])

kurtosis(TK.g[TK.g == 5, ret])
# sharpe ratio
#(((1 + mean(TK.g[TK.g == 5, rit])) ^ 12) - 1) / sd(TK.g[TK.g == 5, ret])

(((1 + mean(TK.g[TK.g == 5, ret])) ^ 12) - (1 + mean(TK.g[TK.g == 5, rf]))) / sd(TK.g[TK.g == 5, ret])

(mean(TK.g[TK.g == 5, rit])) / sd(TK.g[TK.g == 5, ret]) * (12) ^ (1 / 2)


############################################ PW
PW.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, PW.g)
	][!is.na(ret)]

PW.g <- PW.g[, year := year(date)
	][, month := month(date)]
PW.g <- four[PW.g, on = .(year, month)
	][, rit := ret - rf]

low <- PW.g[PW.g == 1]
high <- PW.g[PW.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(PW.g[PW.g == 1, ret])
t.test(PW.g[PW.g == 5, ret])
t.test(dif[, dif])

sd(PW.g[PW.g == 5, ret])

skewness(PW.g[PW.g == 5, ret])

kurtosis(PW.g[PW.g == 5, ret])
# sharpe ratio
# (((1 + mean(TK.g[TK.g == 5, rit])) ^ 12) - 1) / sd(TK.g[TK.g == 5, ret])

(((1 + mean(PW.g[PW.g == 5, ret])) ^ 12) - (1 + mean(PW.g[PW.g == 5, rf]))) / sd(PW.g[PW.g == 5, ret])


############################################ LA
LA.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, LA.g)
	][!is.na(ret)]

LA.g <- LA.g[, year := year(date)
	][, month := month(date)]
LA.g <- four[LA.g, on = .(year, month)
	][, rit := ret - rf]

low <- LA.g[LA.g == 1]
high <- LA.g[LA.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(LA.g[LA.g == 1, ret])
t.test(LA.g[LA.g == 5, ret])
t.test(dif[, dif])

sd(LA.g[LA.g == 5, ret])

skewness(LA.g[LA.g == 5, ret])

kurtosis(LA.g[LA.g == 5, ret])
# sharpe ratio
# (((1 + mean(TK.g[TK.g == 5, rit])) ^ 12) - 1) / sd(TK.g[TK.g == 5, ret])

(((1 + mean(LA.g[LA.g == 5, ret])) ^ 12) - (1 + mean(LA.g[LA.g == 5, rf]))) / sd(LA.g[LA.g == 5, ret])


############################################ CC
CC.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, CC.g)
	][!is.na(ret)]

CC.g <- CC.g[, year := year(date)
	][, month := month(date)]
CC.g <- four[CC.g, on = .(year, month)
	][, rit := ret - rf]

low <- CC.g[CC.g == 1]
high <- CC.g[CC.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(CC.g[CC.g == 1, ret])
t.test(CC.g[CC.g == 5, ret])
t.test(dif[, dif])

sd(CC.g[CC.g == 5, ret])

skewness(CC.g[CC.g == 5, ret])

kurtosis(CC.g[CC.g == 5, ret])
# sharpe ratio
# (((1 + mean(TK.g[TK.g == 5, rit])) ^ 12) - 1) / sd(TK.g[TK.g == 5, ret])

(((1 + mean(CC.g[CC.g == 5, ret])) ^ 12) - (1 + mean(CC.g[CC.g == 5, rf]))) / sd(CC.g[CC.g == 5, ret])



####################################################################net flow and porpect value
load("3clean.RData")
data <- clean
data <- data[, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

zuihou <- data[, .(id, year, sem, month, TK, PW, LA, CC)
	][month == 6 | month == 12
	][, .(id, year, sem, TK, PW, LA, CC)]
setnames(zuihou, c("TK", "PW", "LA", "CC"), c("TK.q", "PW.q", "LA.q", "CC.q"))

#滞后
zuihou <- zuihou[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]

data <- zuihou[data, on = .(id, year, sem)]

data <- data[order(id, date)
	][, TK.g := ntile(TK.q, 5), keyby = .(date)
	][, PW.g := ntile(PW.q, 5), keyby = .(date)
	][, LA.g := ntile(LA.q, 5), keyby = .(date)
	][, CC.g := ntile(TK.q, 5), keyby = .(date)]

t.test(data[TK.g == 5, netflow])
t.test(data[TK.g == 5, netflow], data[TK.g == 1, netflow], paired = FALSE)

t.test(data[PW.g == 5, netflow])
t.test(data[PW.g == 5, netflow], data[PW.g == 1, netflow], paired = FALSE)

t.test(data[LA.g == 5, netflow])
t.test(data[LA.g == 5, netflow], data[LA.g == 1, netflow], paired = FALSE)

t.test(data[CC.g == 5, netflow])
t.test(data[CC.g == 5, netflow], data[CC.g == 1, netflow], paired = FALSE)
