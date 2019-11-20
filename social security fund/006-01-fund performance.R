# semi-annual
load('fundchar.RData')

# add performance
# past 36 month
load("fund-NAV.RData")
data.NAV <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(ret.f.raw = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

# 四因子
four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

alpha <- four[data.NAV, on = .(year, month)
	][, ret_rf := ret.f.raw - rf
	][, tag := 1:(.N), keyby = .(id)]

ret <- alpha

# 滚动回归 four factor
reg.roll <- list()
for (i in 36:max(ret[, tag])){
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
		ret.f.raw.12 <- prod(ret.f.raw + 1) - 1
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[12:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "ret.f.raw.12")

# syf
syf <- reg.cof[, .(tag, id, ret.f.raw.12)]

ret <- syf[ret, on = .(id, tag)
	][, .(id, year, month, ret.f.raw.12)]

# match
data.NAV <- ret[data.NAV, on = .(id, year, month)]


# sharpe ratio and Sortino Ratio
# past 36 month
ret <- data.NAV

library(moments)
reg.roll <- list()
for (i in 36:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- list(max = max(ret.f.raw), skewness = skewness(ret.f.raw), min = min(ret.f.raw), sd = sd(ret.f.raw), mean_ret = mean(ret.f.raw))
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
ret <- data.NAV[ret.f.raw < 0]

reg.roll <- list()
for (i in 36:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- list(downsd = sd(ret.f.raw))
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
fundper <- data.NAV[, .(id, year, month, sem, mkt.12, ret.f.raw, ret.f.raw.12, alpha_four, max, min, skewness, sd, downsd, mean_ret, sharperatio, sortinoratio)]

save(fundper, file = "fundper.RData")

