# Future risk-adjusted performance
load("variables.RData")

# next 12-month alpha
# raw return
load('fund-NAV.RData')

# alpha
factor <- fread("C:/Users/shenfan/Desktop/csr/基金data/更新至2019-07-05-three_four_five_factor_daily/three_four_five_factor_daily/fivefactor_daily.csv")
setnames(factor, "trddy", "date")
factor <- factor[, date := as.Date(date)]

NAV <- factor[data.NAV, on = .(date), nomatch = 0
	][, ret_rf := AdjustedNAVGrowth - rf
	][, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][year > 2008]

# tag
tags <- variables[, .(id, year, sem)
	][, tag := 1:.N, keyby = .(id)]

# match
data <- tags[NAV, on = .(id, year, sem)]

# 滚动
reg.roll <- list()
for (i in 2:20) {
	reg.roll[[i]] <- data[tag >= i - 1 & tag <= i, {
		I <- coef(lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma))[1] 
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:20, reg.roll)
roll <- roll[2:20]

reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "month12alpha")

# match
variables <- tags[variables, on = .(id, year, sem)]

variables <- reg.cof[variables, on = .(id, tag)]


# next
variables <- variables[order(id, year, sem)
	][, next12monthalpha := shift(month12alpha, n = 2, fill = NA, type = "lead"), keyby = .(id)
	][, nextalpha_5 := shift(alpha_5, n = 1, fill = NA, type = "lead"), keyby = .(id)]

###############
data <- variables[order(id, date)
	][, .(id, date, category1, category2, CSR1, CSR2, raw_return, alpha_capm, alpha_3, alpha_4, alpha_5, alpha_6, Rsquare, vol, numstock, age, size, flow, fee, turnover, next12monthalpha, nextalpha_5)]

# Winsorize
data <- data[, colnames(data[, 17:20]) := lapply(.SD[, 17:20], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# 滞后
data <- data[, CSR1.1 := shift(CSR1, n = 1, fill = NA, type = 'lag'), keyby = .(id)
	][, size := log(size)
	][, year := year(date)
	][, numstock := log(numstock)
	][, nextalpha_5 := nextalpha_5 * 100
	][, next12monthalpha := next12monthalpha * 100]

# reg
reg1 <- lm(nextalpha_5 ~ CSR1.1 + factor(year) + factor(category2), data)

reg2 <- lm(nextalpha_5 ~ CSR1.1 + flow + vol + Rsquare + size + numstock + fee + turnover + age + factor(year) + factor(category2), data)

reg3 <- lm(next12monthalpha ~ CSR1.1 + factor(year) + factor(category2), data)

reg4 <- lm(next12monthalpha ~ CSR1.1 + flow + vol + Rsquare + size + numstock + fee + turnover + age + factor(year) + factor(category2), data)

stargazer(reg1, reg2, reg3, reg4, type = "html", out = "C:/Users/shenfan/Desktop/csr/table/table6.doc", report = ('vc*t'))
