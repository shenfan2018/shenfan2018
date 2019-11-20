load("monthlydata.RData")
TK <- fund.TK[, .(id, year, month, month_return, TK)
	][, TK.g := ntile(TK, 5), keyby = .(year, month)
	][!is.na(TK.g)
	][, .(ret = mean(month_return)), keyby = .(TK.g, year, month)]

high <- TK[TK.g == 5]
low <- TK[TK.g == 1]
dif <- low[high, on = .(year, month)
	][, dif := i.ret - ret
	][, .(year, month, dif)]

# 回归
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, .(year, month, mkt_rf, rf)]

ret <- mkt[dif, on = .(year, month)
	][, ret_rf := dif - rf
	][, tag := seq(1:(.N))]

reg.roll <- list()
for (i in 36:109) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf) %>% coef() %>% as.list()
	},
	]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:109, reg.roll)
roll <- roll[36:109]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

ret <- reg.cof[ret, on = .(tag)]

setnames(ret, 2, "four_alpha")

ret <- ret[, .(year, month, four_alpha)]

alpha <- ret

############################################## 
alpha <- alpha[, alpha.1 := shift(four_alpha, n = 1, fill = NA, type = "lead")]

# 滞后两期
for (j in 1:11) {
	ret <- alpha

	ret <- ret[order(year, month)
	][, tag := seq(1:(.N))]

	reg.roll <- list()
	for (i in (j + 1):109) {
		reg.roll[[i]] <- ret[tag >= i - j & tag <= i, {
			j <- mean(alpha.1)
		},
		]
		rbindlist(reg.roll)
	}

	roll <- data.table(tag = 1:168, reg.roll)
	roll <- roll[1:168]
	reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

	alpha <- reg.cof[ret, on = .(tag)]

}

setnames(alpha, 3:13, c("alpha.12", "alpha.11", "alpha.10", "alpha.9", "alpha.8", "alpha.7", "alpha.6", "alpha.5", "alpha.4", "alpha.3", "alpha.2"))

alpha <- alpha[, .(TK.g, year, month, four_alpha, alpha.1, alpha.2, alpha.3, alpha.4, alpha.5, alpha.6, alpha.7, alpha.8, alpha.9, alpha.10, alpha.11, alpha.12)]








































four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, .(year, month, mkt_rf, rf)]


TK <- four[TK, on = .(year, month), nomatch = 0]

# 回归
# β 个股过去36个月超额收益率回归到市场的超额收益率
TK <- TK[, tag := seq(1:(.N)), keyby = TK.g]

ret <- TK[, ret_rf := ret - rf]

reg.roll <- list()
for (i in 36:109) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf) %>% coef() %>% as.list()
	},
	keyby = .(TK.g)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:109, reg.roll)
roll <- roll[36:109]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

ret <- reg.cof[ret, on = .(tag, TK.g)]

setnames(ret, 3, "four_alpha")

ret <- ret[, .(TK.g, year, month, four_alpha)]

alpha <- ret

###########################################################
# 滞后一期
alpha <- alpha[, alpha.1 := shift(four_alpha, n = 1, fill = NA, type = "lead"), keyby = .(TK.g)]

# 滞后两期
for (j in 1:11) {
	ret <- alpha

	ret <- ret[order(TK.g, year, month)
	][, tag := seq(1:(.N)), keyby = .(TK.g)]

	reg.roll <- list()
	for (i in (j + 1):168) {
		reg.roll[[i]] <- ret[tag >= i - j & tag <= i, {
			j <- mean(alpha.1)
		},
		keyby = .(TK.g)]
		rbindlist(reg.roll)
	}

	roll <- data.table(tag = 1:168, reg.roll)
	roll <- roll[1:168]
	reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

	alpha <- reg.cof[ret, on = .(tag, TK.g)]

}

setnames(alpha, 3:13, c("alpha.12", "alpha.11", "alpha.10", "alpha.9", "alpha.8", "alpha.7", "alpha.6", "alpha.5", "alpha.4", "alpha.3", "alpha.2"))

alpha <- alpha[, .(TK.g, year, month, four_alpha, alpha.1, alpha.2, alpha.3, alpha.4, alpha.5, alpha.6, alpha.7, alpha.8, alpha.9, alpha.10, alpha.11, alpha.12)]


alpha <- alpha[, alpha.2 := shift(alpha.2, n = 1, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.3 := shift(alpha.3, n = 2, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.4 := shift(alpha.4, n = 3, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.5 := shift(alpha.5, n = 4, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.6 := shift(alpha.6, n = 5, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.7 := shift(alpha.7, n = 6, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.8 := shift(alpha.8, n = 7, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.9 := shift(alpha.9, n = 8, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.10 := shift(alpha.10, n = 9, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.11 := shift(alpha.11, n = 10, fill = NA, type = "lead"), keyby = .(TK.g)
	][, alpha.12 := shift(alpha.12, n = 11, fill = NA, type = "lead"), keyby = .(TK.g)]

data <- alpha

data <- melt(data, id = c("TK.g", "year", "month"), measures = c("alpha.1", "alpha.2", "alpha.3", "alpha.4", "alpha.5", "alpha.6", "alpha.7", "alpha.8", "alpha.9", "alpha.10", "alpha.11", "alpha.12"))

data <- data[!is.na(value)]

low <- data[TK.g == 1]
high <- data[TK.g == 5]
dif <- low[high, on = .(variable, year, month)
	][, dif := i.value - value]


# a <- dif[, .(dif = mean(dif)), keyby = .(variable)
#	][-1
#	][, period := seq(1:(.N))]

a <- dif[, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3]), keyby = .(variable)
	][-1
	][, period := seq(1:(.N))]


ggplot(a, aes(x = period)) +
	geom_line(aes(y = t)) +
	theme_bw() +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 8),
#	legend.title = element_blank(),
	legend.position = c(0.15, 0.85),
)
