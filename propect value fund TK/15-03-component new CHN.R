# tk separate to performance and speculate
# TK ~ raw return + alpha + max + skewness
load("fundfactor.RData")

# max and skewness rolling
alpha <- alpha[, tag := 1:(.N), keyby = .(id)
	][, .(id, year, month, tag, TK, month_return, alpha_four)]

# ¹ö¶¯»Ø¹é max, skewness
library(moments)
reg.roll <- list()
for (i in 36:max(alpha[, tag])) {
	reg.roll[[i]] <- alpha[tag >= i - 35 & tag <= i, {
		I <- list(max = max(month_return), skewness = skewness(month_return), min = min(month_return), sd = sd(month_return), mean_ret = mean(month_return))
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(alpha[, tag]), reg.roll)
roll <- roll[36:max(alpha[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

# syf
syf <- reg.cof[, .(tag, id, max, min, skewness, sd, mean_ret)]
# match 
alpha <- syf[alpha, on = .(id, tag)
	][, SR := (mean_ret / sd) * sqrt(12)]

## match 2
#load("monthlydata.RData")
#fund.TK <- fund.TK[, .(id, year, month, netflow, churn.rate + logfund_size, logfund_age)]
#alpha <- fund.TK[alpha, on = .(id, year, month)]

# match 3 ability
load("ability.RData")
alpha <- alpha[, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]
alpha <- ability[alpha, on = .(id, year, sem)]

# match residuals
vol <- alpha[, .(id, year, month, month_return)]
# add factor
four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
vol <- four[vol, on = .(year, month)
	][, ret_rf := month_return - rf]
vol <- vol[, idiosyncraticvol := abs(residuals(lm(ret_rf ~ mkt_rf + smb + hml + umd)))
	][, .(id, year, month, idiosyncraticvol)]
# match 4
alpha <- vol[alpha, on = .(id, year, month)]

##########################################
#data <- alpha[, .(id, year, month, TK, month_return, alpha_four, SR, timing, picking, fundgap, max, skewness, min, idiosyncraticvol)
	#][!is.na(max)
	#][!is.nan(TK)
	#][, a0 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four))[1]
	#][, a1 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four))[2]
	#][, a2 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four))[3]
	#][, a3 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four))[4]
	#][, a4 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four))[5]
	#][, a5 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four))[6]
	#][, component1 := a1 * SR + a2 * timing + a3 * picking + a4 * fundgap + a5 * alpha_four
	#][, residual1 := TK - component1 - a0]
	##][, a3 := coef(lm(TK ~ max + skewness))[2]
	##][, a4 := coef(lm(TK ~ max + skewness))[3]
	##][, component2 := a3 * max + a4 * skewness
	##][, residual2 := TK - component2]

# choose
data <- alpha[, .(id, year, month, TK, month_return, alpha_four, SR, timing, picking, fundgap, max, skewness, min, idiosyncraticvol)
	][!is.na(max)
	][!is.nan(TK)
	][, a1 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four + max + min + skewness + idiosyncraticvol))[2]
	][, a2 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four + max + min + skewness + idiosyncraticvol))[3]
	][, a3 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four + max + min + skewness + idiosyncraticvol))[4]
	][, a4 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four + max + min + skewness + idiosyncraticvol))[5]
	][, a5 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four + max + min + skewness + idiosyncraticvol))[6]
	][, a6 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four + max + min + skewness + idiosyncraticvol))[7]
	][, a7 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four + max + min + skewness + idiosyncraticvol))[8]
	][, a8 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four + max + min + skewness + idiosyncraticvol))[9]
	][, a9 := coef(lm(TK ~ SR + timing + picking + fundgap + alpha_four + max + min + skewness + idiosyncraticvol))[10]
	][, component1 := a1 * SR + a2 * timing + a3 * picking + a4 * fundgap + a5 * alpha_four
	][, component2 := a5 * max + a6 * min + a7 * skewness + a8 * idiosyncraticvol
	][, residual := TK - component1 - component2]

# sort
data <- data[, component1.g := ntile(residual, 5), keyby = .(year, month)
	][, .(id, year, month, month_return, component1.g)
	][order(id, year, month)
	][, component1.g.1 := shift(component1.g, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][!is.na(component1.g.1)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month, component1.g.1)]

# add factor
four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

data <- four[data, on = .(year, month)
	][, rit := ret - rf]

# 2. lm
a <- data[, .(raw_return = mean(ret), raw_t = t.test(ret)$statistic, alpha_capm = coef(lm(rit ~ mkt_rf))[1], t_capm = summary(lm(rit ~ mkt_rf))$coef[1, 3], alpha_three = coef(lm(rit ~ mkt_rf + smb + hml))[1], t_three = summary(lm(rit ~ mkt_rf + smb + hml))$coef[1, 3], alpha_four = coef(lm(rit ~ mkt_rf + smb + hml + umd))[1], t_four = summary(lm(rit ~ mkt_rf + smb + hml + umd))$coef[1, 3], alpha_five = coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma))[1], t_five = summary(lm(rit ~ mkt_rf + smb + hml + rmw + cma))$coef[1, 3], alpha_six = coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t_six = summary(lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[1, 3]), keyby = .(component1.g.1)]

write.csv(a, "C://Users//shenfan//Desktop//mydata.csv")

# dif
a <- data[, rit2 := shift(rit, n = 4, fill = NA, type = "lead")
	][, dif := rit2 - rit
	][component1.g.1 == 1]

a <- a[, .(raw_return = mean(dif), raw_t = t.test(dif)$statistic, alpha_capm = coef(lm(dif ~ mkt_rf))[1], t_capm = summary(lm(dif ~ mkt_rf))$coef[1, 3], alpha_three = coef(lm(dif ~ mkt_rf + smb + hml))[1], t_three = summary(lm(dif ~ mkt_rf + smb + hml))$coef[1, 3], alpha_four = coef(lm(dif ~ mkt_rf + smb + hml + umd))[1], t_four = summary(lm(dif ~ mkt_rf + smb + hml + umd))$coef[1, 3], alpha_five = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma))[1], t_five = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma))$coef[1, 3], alpha_six = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t_six = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[1, 3]), keyby = .(component1.g.1)]

write.csv(a, "C://Users//shenfan//Desktop//mydata2.csv")