# add performance
# past 36 month
load("social_ret.RData")

# 四因子
four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

alpha <- four[social.ret, on = .(year, month)
	][, ret_rf := ret.s - rf
	][, tag := 1:(.N), keyby = .(fund)]

ret <- alpha

# 滚动回归 four alpha
reg.roll <- list()
for (i in 36:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf + hml + smb + umd) %>% coef() %>% as.list()
	},
	, keyby = .(fund)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[36:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_four")

# syf
syf <- reg.cof[, .(tag, fund, alpha_four)]

alpha <- syf[alpha, on = .(fund, tag)
	][, .(fund, year, month, alpha_four)]

# match
social.ret <- alpha[social.ret, on = .(fund, year, month)]


# 过去12个月的收益累计
ret <- social.ret[, tag := seq(1:(.N)), keyby = .(fund)]

reg.roll <- list()
for (i in 12:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 11 & tag <= i, {
		ret.s.12 <- prod(ret.s + 1) - 1
	},
	, keyby = .(fund)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[12:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "ret.s.12")

# syf
syf <- reg.cof[, .(tag, fund, ret.s.12)]

ret <- syf[ret, on = .(fund, tag)
	][, .(fund, year, month, ret.s.12)]

# match
social.ret <- ret[social.ret, on = .(fund, year, month)]

# sharpe ratio and Sortino Ratio
# past 36 month
ret <- social.ret

library(moments)
reg.roll <- list()
for (i in 36:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- list(max = max(ret.s), skewness = skewness(ret.s), min = min(ret.s), sd = sd(ret.s), mean_ret = mean(ret.s))
	},
	, keyby = .(fund)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[36:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]


# match
social.ret <- reg.cof[social.ret, on = .(fund, tag)]

# <0的sd
ret <- social.ret[ret.s < 0]

reg.roll <- list()
for (i in 36:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- list(downsd = sd(ret.s))
	},
	, keyby = .(fund)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[36:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

# syf
social.ret <- reg.cof[social.ret, on = .(fund, tag)]

social.ret <- social.ret[, sharperatio := (mean_ret / sd) * sqrt(12)
	][, sortinoratio := (mean_ret / downsd) * sqrt(12)]

# performance
socialper <- social.ret[, .(fund, year, month, sem, ret.s, ret.s.12, alpha_four, max, min, skewness, sd, downsd, mean_ret, sharperatio, sortinoratio)]


save(socialper, file = "socialper.RData")
