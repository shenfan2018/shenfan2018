# tk separate to performance and speculate
# TK ~ raw return + alpha + max + skewness
load("USfundfactor.RData")

# max and skewness rolling
alpha <- alpha[, tag := 1:(.N), keyby = .(id)]

# 滚动回归 max
reg.roll <- list()
for (i in 36:168) {
	reg.roll[[i]] <- alpha[tag >= i - 35 & tag <= i, {
		I <- max(month_return)
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[36:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "max")

# syf
syf <- reg.cof[, .(tag, id, max)]

# match 
alpha <- syf[alpha, on = .(id, tag)]

# 滚动回归 skewness
library(moments)
reg.roll <- list()
for (i in 36:168) {
	reg.roll[[i]] <- alpha[tag >= i - 35 & tag <= i, {
		I <- skewness(month_return)
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[36:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "skewness")

# syf
syf <- reg.cof[, .(tag, id, skewness)]

# match 
alpha <- syf[alpha, on = .(id, tag)]

# add TK
load("USTK.RData")
setnames(fund.TK, "fund.id", "id")

# alpha 日期更改
alpha <- alpha[, year := as.numeric(substring(Date, 1, 4))
	][, month := as.numeric(substring(Date, 5, 6))
	][, id := as.character(id)]

# match
alpha <- fund.TK[alpha, on = .(id, year, month)]

##########################################
data <- alpha[, .(id, year, month, TK, month_return, alpha_CAPM, alpha_three, alpha_four, alpha_five, alpha_six, max, skewness)
	][!is.na(max)
	][!is.nan(TK)
	][, a1 := coef(lm(TK ~ month_return + alpha_four + max + skewness))[2]
	][, a2 := coef(lm(TK ~ month_return + alpha_four + max + skewness))[3]
	][, a3 := coef(lm(TK ~ month_return + alpha_four + max + skewness))[4]
	][, a4 := coef(lm(TK ~ month_return + alpha_four + max + skewness))[5]
	][, component1 := a1 * month_return + a2 * alpha_four
	][, component2 := a3 * max + a4 * skewness]

# sort
data <- data[, lapply(colnames(data[, 5:10]), str_c, ".1") %>% unlist() := lapply(.SD[, 4:9], shift, n = 1L, type = "lead"), keyby = .(id)
	][, component2.g := ntile(component2, 5), keyby = .(year, month)
	][, .(id, year, month, component2, component2.g, month_return.1, alpha_CAPM.1, alpha_three.1, alpha_four.1, alpha_five.1, alpha_six.1)]

data <- melt(data, id.vars = colnames(data[, 1:5]), measures.vars = colnames(data[, 6:11]))

group <- data[, .(ret = mean(value)), keyby = .(year, month, variable, component2.g)
	][!is.na(ret)
	][, .(t = coeftest(lm(ret ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(ret)$estimate), keyby = .(variable, component2.g)]

write.csv(group, "C://Users//shenfan//Desktop//mydatam.csv")

# dif
group <- data[, .(ret = mean(value)), keyby = .(year, month, variable, component2.g)
	][!is.na(ret)]

low <- group[component2.g == 1]
high <- group[component2.g == 5]
dif <- low[high, on = .(year, month, variable)
	][, dif := i.ret - ret
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(variable)]

write.csv(dif, "C://Users//shenfan//Desktop//mydatam2.csv")
