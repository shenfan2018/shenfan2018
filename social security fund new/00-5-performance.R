# social
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

# 过去12个月mkt
four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
ret <- four[, tag := seq(1:(.N))
	][, mkt := mkt_rf + rf]

syf <- vector()
for (i in 12:max(ret[, tag])) {
	syf[i] <- ret[tag >= i - 11 & tag <= i, {
		mkt.12 <- prod(mkt + 1) - 1
	},]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
setnames(syf, "syf", "mkt.12")
ret <- syf[ret, on = .(tag)
	][, .(year, month, mkt.12)]

social.ret <- ret[social.ret, on = .(year, month)]

# performance
socialper <- social.ret[, .(fund, year, month, mkt.12, ret.s, ret.s.12, alpha_four, max, min, skewness, sd, downsd, mean_ret, sharperatio, sortinoratio)]

# 4731 * 15
save(socialper, file = "socialper.RData")

###################################################################### mutual fund
# semi-annual
load('fundchar.RData')

# add performance
# past 36 month
load("fund_ret.RData")
data.NAV <- fund.ret

# 四因子
four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

alpha <- four[data.NAV, on = .(year, month)
	][, ret_rf := ret.f - rf
	][, tag := 1:(.N), keyby = .(id)]

ret <- alpha

# 滚动回归 four factor
reg.roll <- list()
for (i in 36:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf + hml + smb + umd) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[36:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_four")

# syf
syf <- reg.cof[, .(tag, id, alpha_four)]

alpha <- syf[alpha, on = .(id, tag)
	][, .(id, year, month, alpha_four)]

# match
data.NAV <- alpha[data.NAV, on = .(id, year, month)]


# 过去12个月的收益累计
ret <- data.NAV[, tag := seq(1:(.N)), keyby = .(id)]

reg.roll <- list()
for (i in 12:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 11 & tag <= i, {
		ret.f.12 <- prod(ret.f + 1) - 1
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[12:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "ret.f.12")

# syf
syf <- reg.cof[, .(tag, id, ret.f.12)]

ret <- syf[ret, on = .(id, tag)
	][, .(id, year, month, ret.f.12)]

# match
data.NAV <- ret[data.NAV, on = .(id, year, month)]


# sharpe ratio and Sortino Ratio
# past 36 month
ret <- data.NAV

library(moments)
reg.roll <- list()
for (i in 36:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- list(max = max(ret.f), skewness = skewness(ret.f), min = min(ret.f), sd = sd(ret.f), mean_ret = mean(ret.f))
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[36:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]


# match
data.NAV <- reg.cof[data.NAV, on = .(id, tag)]


# <0的sd
ret <- data.NAV[ret.f < 0]

reg.roll <- list()
for (i in 36:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- list(downsd = sd(ret.f))
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[36:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

# syf
data.NAV <- reg.cof[data.NAV, on = .(id, tag)]

data.NAV <- data.NAV[, sharperatio := (mean_ret / sd) * sqrt(12)
	][, sortinoratio := (mean_ret / downsd) * sqrt(12)]

# 过去12个月mkt
four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
ret <- four[, tag := seq(1:(.N))
	][, mkt := mkt_rf + rf]

syf <- vector()
for (i in 12:max(ret[, tag])) {
	syf[i] <- ret[tag >= i - 11 & tag <= i, {
		mkt.12 <- prod(mkt + 1) - 1
	},]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
setnames(syf, "syf", "mkt.12")
ret <- syf[ret, on = .(tag)
	][, .(year, month, mkt.12)]

data.NAV <- ret[data.NAV, on = .(year, month)]

# performance
fundper <- data.NAV[, .(id, year, month, mkt.12, ret.f, ret.f.12, alpha_four, max, min, skewness, sd, downsd, mean_ret, sharperatio, sortinoratio)]

save(fundper, file = "fundper.RData")

####################################################### factor alpha
load("fund_ret.RData")

# 四因子
four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

alpha <- four[fund.ret, on = .(year, month)
	][, ret_rf := ret.f - rf
	][, tag := 1:(.N), keyby = .(id)]

ret <- alpha

# 滚动回归 four factor
reg.roll <- list()
for (i in 36:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- list(alpha_capm = coef(lm(ret_rf ~ mkt_rf))[1], alpha_three = coef(lm(ret_rf ~ mkt_rf + smb + hml))[1], alpha_four = coef(lm(ret_rf ~ mkt_rf + smb + hml + umd))[1], alpha_five = coef(lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma))[1], alpha_six = coef(lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd))[1])
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[36:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]


# syf
alpha <- reg.cof[alpha, on = .(id, tag)
	][, .(id, year, month, quarter, ret.f, ret.f_mkt, alpha_capm, alpha_three, alpha_four, alpha_five, alpha_six)]

fund.alpha <- alpha
save(fund.alpha, file = "fundalpha.RData")
