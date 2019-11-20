load("monthlydata.RData")

# Winsorize
fund.TK <- fund.TK[, sem := NULL
	][, colnames(fund.TK[, 9:11]) := lapply(.SD[, 9:11], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

data <- fund.TK[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][!is.nan(TK)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, .(id, year, month, date, month_return, month_return.1, TK.g)]

data <- data[order(year, month, month_return)
	][, pm := seq(1:.N), keyby = .(year, month)
	][, month_pm := pm / max(pm), keyby = .(year, month)
	][order(year, month, month_return.1)
	][, pm := seq(1:.N), keyby = .(year, month)
	][, month_pm.1 := pm / max(pm), keyby = .(year, month)]

# panel a
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

dif <- TK.g[, i.ret := shift(ret, n = 4, fill = NA, type = "lead"), keyby = .(date)
	][, dif := i.ret - ret
	][!is.na(dif)]


t.test(TK.g[TK.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

# factor
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(year, month, TK.g)
	][!is.na(ret)
	][, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
TK.g <- four[TK.g, on = .(year, month)
	][, rit := ret - rf]

reg5 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 5])
reg4 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 4])
reg3 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 5])
reg2 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 2])
reg1 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 1])

# difference
TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(year, month, TK.g)
	][!is.na(ret)
	][, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(year, month)
	][, dif := i.ret - ret]
dif <- four[dif, on = .(year, month)]

regd <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd , dif)

stargazer(reg1, reg2, reg3, reg4, reg5, regd, type = "html", out = "C:/Users/shenfan/Desktop/prospect value/new/factor.doc")


# panel b
data <- fund.TK

data <- data[order(id, year, month)
	][!is.nan(TK)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, .(id, year, month, date, quarter_return.1, TK.g)]

TK.g <- data[, .(ret = mean(quarter_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)
	][, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[TK.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

# factor
TK.g <- data[, .(ret = mean(quarter_return.1)), keyby = .(year, month, TK.g)
	][!is.na(ret)
	][, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
TK.g <- four[TK.g, on = .(year, month)
	][, rit := ret - rf]

reg5 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 5])
reg4 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 4])
reg3 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 5])
reg2 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 2])
reg1 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[TK.g == 1])

# difference
TK.g <- data[, .(ret = mean(quarter_return.1)), keyby = .(year, month, TK.g)
	][!is.na(ret)
	][, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(year, month)
	][, dif := i.ret - ret]
dif <- four[dif, on = .(year, month)]

regd <- lm(dif ~ mkt_rf + smb + hml + smb + hml + rmw + cma + umd, dif)

stargazer(reg1, reg2, reg3, reg4, reg5, regd, type = "html", out = "C:/Users/shenfan/Desktop/prospect value/new/factorq.doc")


#####################################################################
load("monthlydata.RData")
data <- fund.TK

# table 3
data <- data[order(id, year, month)
	][!is.nan(TK)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, .(id, year, month, date, month_return, TK.g)
	][order(id, year, month)
	][, TK.g.1 := shift(TK.g, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][!is.na(TK.g.1)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month, TK.g.1)]

four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
data <- four[data, on = .(year, month)
	][, rit := ret - rf]

# 2. lm
a <- data[, .(raw_return = mean(ret), raw_t = t.test(ret)$statistic, alpha_capm = coef(lm(rit ~ mkt_rf))[1], t_capm = summary(lm(rit ~ mkt_rf))$coef[1, 3], alpha_three = coef(lm(rit ~ mkt_rf + smb + hml))[1], t_three = summary(lm(rit ~ mkt_rf + smb + hml))$coef[1, 3], alpha_four = coef(lm(rit ~ mkt_rf + smb + hml + umd))[1], t_four = summary(lm(rit ~ mkt_rf + smb + hml + umd))$coef[1, 3], alpha_five = coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma))[1], t_five = summary(lm(rit ~ mkt_rf + smb + hml + rmw + cma))$coef[1, 3], alpha_six = coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t_six = summary(lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[1, 3]), keyby = .(TK.g.1)]

write.csv(a, "C://Users//shenfan//Desktop//mydata.csv")

# dif
a <- data[, rit2 := shift(rit, n = 4, fill = NA, type = "lead")
	][, dif := rit2 - rit
	][TK.g.1 == 1]

a <- a[, .(raw_return = mean(dif), raw_t = t.test(dif)$statistic, alpha_capm = coef(lm(dif ~ mkt_rf))[1], t_capm = summary(lm(dif ~ mkt_rf))$coef[1, 3], alpha_three = coef(lm(dif ~ mkt_rf + smb + hml))[1], t_three = summary(lm(dif ~ mkt_rf + smb + hml))$coef[1, 3], alpha_four = coef(lm(dif ~ mkt_rf + smb + hml + umd))[1], t_four = summary(lm(dif ~ mkt_rf + smb + hml + umd))$coef[1, 3], alpha_five = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma))[1], t_five = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma))$coef[1, 3], alpha_six = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t_six = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[1, 3]), keyby = .(TK.g.1)]


a <- a[, .(raw_return = mean(dif), raw_t = t.test(dif)$statistic, alpha_capm = coef(lm(dif ~ mkt_rf))[1], t_capm = summary(lm(dif ~ mkt_rf))$coef[1, 3], alpha_three = coef(lm(dif ~ mkt_rf + smb + hml))[1], t_three = summary(lm(dif ~ mkt_rf + smb + hml))$coef[1, 3], alpha_four = coef(lm(dif ~ mkt_rf + smb + hml + umd))[1], t_four = summary(lm(dif ~ mkt_rf + smb + hml + umd))$coef[1, 3], alpha_five = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma))[1], t_five = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma))$coef[1, 3], alpha_six = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t_six = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[1, 3]), keyby = .(TK.g.1)]




a <- a[, .(raw_return = t.test(dif)$estimate, raw_t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], alpha_capm = coef(lm(dif ~ mkt_rf))[1], t_capm = coeftest(lm(dif ~ mkt_rf), vcov. = NeweyWest)[, 3], alpha_three = coef(lm(dif ~ mkt_rf + smb + hml))[1], t_three = coeftest(lm(dif ~ mkt_rf + smb + hml), vcov. = NeweyWest)[, 3], alpha_four = coef(lm(dif ~ mkt_rf + smb + hml + umd))[1], t_four = coeftest(lm(dif ~ mkt_rf + smb + hml + umd), vcov. = NeweyWest)[, 3], alpha_five = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma))[1], t_five = coeftest(lm(dif ~ mkt_rf + smb + hml + rmw + cma), vcov. = NeweyWest)[, 3], alpha_six = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t_six = coeftest(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd), vcov. = NeweyWest)[, 3]), keyby = .(TK.g.1)]

