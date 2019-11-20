# beta 五因子
load('fund-NAV.RData')

# alpha
factor <- fread("C:/Users/shenfan/Desktop/csr/基金data/更新至2019-07-05-three_four_five_factor_daily/three_four_five_factor_daily/fivefactor_daily.csv")
setnames(factor, "trddy", "date")
factor <- factor[, date := as.Date(date)]

NAV <- factor[data.NAV, on = .(date), nomatch = 0
	][!is.na(AdjustedNAVGrowth)
	][, ret_rf := AdjustedNAVGrowth - rf
	][, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][, n := .N, keyby = .(id, year, sem)
	][n > 30
	][year > 2008]

beta <- NAV[, I <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma) %>% coef() %>% as.list(), keyby = .(id, year, sem)]

##########################################################################

load("variables.RData")
data <- beta[variables, on = .(id, year, sem)]

data <- data[order(id, date)
	][, .(id, date, category1, category2, CSR1, CSR2, raw_return, alpha_capm, alpha_3, alpha_4, alpha_5, alpha_6, Rsquare, vol, numstock, age, size, flow, fee, turnover, mkt_rf, smb, hml, rmw, cma)]

# Winsorize
data <- data[, colnames(data[, 17:20]) := lapply(.SD[, 17:20], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# 滞后
data <- data[, CSR1.1 := shift(CSR1, n = 1, fill = NA, type = 'lag'), keyby = .(id)
	][, size := log(size)
	][, year := year(date)
	][, numstock := log(numstock)
	][, alpha_5 := alpha_5 * 100]

# reg
reg1 <- lm(CSR1.1 ~ alpha_5 + mkt_rf + smb + hml + rmw + cma + factor(year) + factor(category2), data)

reg2 <- lm(CSR1.1 ~ alpha_5 + mkt_rf + smb + hml + rmw + cma + flow + vol + Rsquare + size + numstock + fee + turnover + age + factor(year) + factor(category2), data)

stargazer(reg1, reg2, type = "html", out = "C:/Users/shenfan/Desktop/csr/table/table5_2.doc", report = ('vc*t'))
