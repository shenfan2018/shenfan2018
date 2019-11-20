# Monthly six-factor model analysis of value weighted fund returns of sample funds based on CSR rankings
load("fund-NAV.RData")

# monthly
data.m <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

# CSR # 这里就选择了CSR1
load("variablesfund.RData")
variables <- variables[, .(id, date, CSR1, size)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)]
# CSR要滞后
variables <- variables[, CSR1.1 := shift(CSR1, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][, .(id, year, sem, date, CSR1, CSR1.1, size)]

# match
data <- variables[data.m, on = .(id, year, sem), nomatch = 0]

## value-weighted
#data <- data[!is.na(size)
	#][!is.na(CSR1.1)
	#][, CSR1.g := ntile(CSR1.1, 5), keyby = .(year, month)
	#][, vw := size / sum(size), keyby = .(year, month, CSR1.g)
	#][, .(ret = sum(month_return * vw)), keyby = .(year, month, CSR1.g)]

# equal weighted
data <- data[!is.na(CSR1.1)
	][, CSR1.g := ntile(CSR1.1, 5), keyby = .(year, month)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month, CSR1.g)]

# factor
four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")

data <- four[data, on = .(year, month)
	][, ret_rf := ret - rf]

# reg
reg1 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, data[CSR1.g == 1]) 
reg2 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, data[CSR1.g == 2])
reg3 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, data[CSR1.g == 3])
reg4 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, data[CSR1.g == 4])
reg5 <- lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma + umd, data[CSR1.g == 5])

# dif reg
dif <- data[, ret.2 := shift(ret, n = 4, fill = NA, type = "lead"), keyby = .(year, month)
	][CSR1.g == 1
	][, dif := ret.2 - ret]

reg6 <- lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, dif)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, type = "html", out = "C:/Users/shenfan/Desktop/csr/table/table4_2.doc", report = ('vc*t'))








