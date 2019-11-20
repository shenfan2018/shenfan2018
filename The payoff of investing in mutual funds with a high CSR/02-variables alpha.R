# alpha R square return, R square choose 4
# raw return
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
	][year > 2008
	][, .(raw_return = prod(AdjustedNAVGrowth + 1) - 1, alpha_capm = coef(lm(ret_rf ~ mkt_rf))[1], alpha_3 = coef(lm(ret_rf ~ mkt_rf + smb + hml))[1], alpha_4 = coef(lm(ret_rf ~ mkt_rf + smb + hml + umd))[1], alpha_5 = coef(lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma))[1], alpha_6 = coef(lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd))[1], Rsquare = summary(lm(ret_rf ~ mkt_rf + smb + hml + umd))[[8]]), keyby = .(id, year, sem)]

load("variablesfund.RData")
variables <- variables[, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)]
variables <- NAV[variables, on = .(id, year, sem)]

save(variables, file = "variables.RData")

