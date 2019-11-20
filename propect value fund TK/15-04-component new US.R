# tk separate to performance and speculate
# TK ~ raw return + alpha + max + skewness
load("USfundfactor.RData")

# max and skewness rolling
alpha <- alpha[, tag := 1:(.N), keyby = .(id)]

# 滚动回归 max
reg.roll <- list()
for (i in 36:max(alpha[, tag])){
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

# add TK
load("USTK.RData")
setnames(fund.TK, "fund.id", "id")

# alpha 日期更改
alpha <- alpha[, year := as.numeric(substring(Date, 1, 4))
	][, month := as.numeric(substring(Date, 5, 6))
	][, id := as.character(id)]

# match
alpha <- fund.TK[alpha, on = .(id, year, month)]

# match 3 ability
# mg6
other <- fread('C:/Users/shenfan/Desktop/prospect value/data US/mg6.csv')
other <- other[, quarter := ifelse(month == 3, 1, ifelse(month == 6, 2, ifelse(month == 9, 3, 4)))
	][, CRSP_FUNDNO := as.character(crsp_fundno)
	][, .(CRSP_FUNDNO, year, quarter, q_timing, q_picking, skill, avg_crkt)]
setnames(other, "CRSP_FUNDNO", "id")
# match
alpha <- alpha[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))]

alpha <- other[alpha, on = .(id, year, quarter)]

# match residuals
vol <- alpha[, .(id, year, month, month_return)]
# add factor
# factor
factor1 <- fread('C:/Users/shenfan/Desktop/prospect value/data US/factor1.csv')
factor1 <- factor1[, Date := substring(dateff, 1, 6)
	][, .(Date, UMD)]
factor2 <- fread('C:/Users/shenfan/Desktop/prospect value/data US/factor2.csv')
factor2 <- factor2[, Date := as.character(Date)
	][, colnames(factor2[, 2:7]) := .SD[, 2:7] / 100]
# match
factor <- factor1[factor2, on = .(Date), nomatch = 0]
rm(factor1, factor2)
factor <- factor[, year := as.numeric(substring(Date, 1, 4))
	][, month := as.numeric(substring(Date, 5, 6))]
# match factor
vol <- factor[vol, on = .(year, month)
	][, ret_rf := month_return - RF]

vol <- vol[, idiosyncraticvol := abs(residuals(lm(ret_rf ~ Mkt_RF + SMB + HML + UMD)))
	][, .(id, year, month, idiosyncraticvol)]
# match 4
alpha <- vol[alpha, on = .(id, year, month)]

# 1991年开始
alpha <- alpha[year > 1990]

##########################################
data <- alpha[, .(id, year, month, TK, month_return, alpha_four, SR, q_timing, q_picking, skill, avg_crkt, max, skewness, min, idiosyncraticvol)
	][!is.na(max)
	][!is.nan(TK)
	][, a1 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[2]
	][, a2 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[3]
	][, a3 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[4]
	][, a4 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[5]
	][, a5 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[6]
	][, a6 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[7]
	][, a7 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[8]
	][, a8 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[9]
	][, a9 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[10]
	][, a10 := coef(lm(TK ~ SR + q_timing + q_picking + skill + avg_crkt + alpha_four + max + min + skewness + idiosyncraticvol))[11]
	][, component1 := a1 * SR + a2 * q_timing + a3 * q_picking + a4 * skill + a5 * avg_crkt + a6 * alpha_four
	][, component2 := a7 * max + a8 * min + a9 * skewness + a10 * idiosyncraticvol
	][, residual := TK - component1 - component2]

# sort
data <- data[, component1.g := ntile(residual, 5), keyby = .(year, month)
	][, .(id, year, month, month_return, component1.g)
	][order(id, year, month)
	][, component1.g.1 := shift(component1.g, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][!is.na(component1.g.1)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month, component1.g.1)]

# factor
# factor
factor1 <- fread('C:/Users/shenfan/Desktop/prospect value/data US/factor1.csv')
factor1 <- factor1[, Date := substring(dateff, 1, 6)
	][, .(Date, UMD)]

factor2 <- fread('C:/Users/shenfan/Desktop/prospect value/data US/factor2.csv')
factor2 <- factor2[, Date := as.character(Date)
	][, colnames(factor2[, 2:7]) := .SD[, 2:7] / 100]
# match
factor <- factor1[factor2, on = .(Date), nomatch = 0]
rm(factor1, factor2)

factor <- factor[, year := as.numeric(substring(Date, 1, 4))
	][, month := as.numeric(substring(Date, 5, 6))]

# match factor
data <- factor[data, on = .(year, month)]

data <- data[, rit := ret - RF]

a <- data[, .(raw_return = mean(ret), raw_t = t.test(ret)$statistic, alpha_capm = coef(lm(rit ~ Mkt_RF))[1], t_capm = summary(lm(rit ~ Mkt_RF))$coef[1, 3], alpha_three = coef(lm(rit ~ Mkt_RF + SMB + HML))[1], t_three = summary(lm(rit ~ Mkt_RF + SMB + HML))$coef[1, 3], alpha_four = coef(lm(rit ~ Mkt_RF + SMB + HML + UMD))[1], t_four = summary(lm(rit ~ Mkt_RF + SMB + HML + UMD))$coef[1, 3], alpha_five = coef(lm(rit ~ Mkt_RF + SMB + HML + RMW + CMA))[1], t_five = summary(lm(rit ~ Mkt_RF + SMB + HML + RMW + CMA))$coef[1, 3], alpha_six = coef(lm(rit ~ Mkt_RF + SMB + HML + RMW + CMA + UMD))[1], t_six = summary(lm(rit ~ Mkt_RF + SMB + HML + RMW + CMA + UMD))$coef[1, 3]), keyby = .(component1.g.1)]

write.csv(a, "C://Users//shenfan//Desktop//mydata.csv")


# dif
# dif
a <- data[, rit2 := shift(rit, n = 4, fill = NA, type = "lead")
	][, dif := rit2 - rit
	][component1.g.1 == 1]

a <- a[, .(raw_return = t.test(dif)$estimate, raw_t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], alpha_capm = coef(lm(dif ~ Mkt_RF))[1], t_capm = summary(lm(dif ~ Mkt_RF))$coef[1, 3], alpha_three = coef(lm(dif ~ Mkt_RF + SMB + HML))[1], t_three = summary(lm(dif ~ Mkt_RF + SMB + HML))$coef[1, 3], alpha_four = coef(lm(dif ~ Mkt_RF + SMB + HML + UMD))[1], t_four = summary(lm(dif ~ Mkt_RF + SMB + HML + UMD))$coef[1, 3], alpha_five = coef(lm(dif ~ Mkt_RF + SMB + HML + RMW + CMA))[1], t_five = summary(lm(dif ~ Mkt_RF + SMB + HML + RMW + CMA))$coef[1, 3], alpha_six = coef(lm(dif ~ Mkt_RF + SMB + HML + RMW + CMA + UMD))[1], t_six = summary(lm(dif ~ Mkt_RF + SMB + HML + RMW + CMA + UMD))$coef[1, 3]), keyby = .(component1.g.1)]

write.csv(a, "C://Users//shenfan//Desktop//mydata2.csv")