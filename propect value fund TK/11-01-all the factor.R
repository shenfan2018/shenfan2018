load("monthlydata.RData")
fund.TK <- fund.TK[, .(id, year, month, TK, month_return)]

four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

alpha <- four[fund.TK, on = .(year, month)
	][, ret_rf := month_return - rf
	][, tag := 1:(.N), keyby = .(id)]

ret <- alpha

# ¹ö¶¯»Ø¹é CAPM
reg.roll <- list()
for (i in 36:168) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[36:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_CAPM")

# syf
syf <- reg.cof[, .(tag, id, alpha_CAPM)]

 ################################################################################FF 3 factor
reg.roll <- list()
for (i in 36:168) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf + hml + smb) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[36:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_three")
reg.cof <- reg.cof[, .(tag, id, alpha_three)]

# match
syf <- reg.cof[syf, on = .(tag, id)]


################################################################################carhart 4 factor
reg.roll <- list()
for (i in 36:168) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf + hml + smb + umd)  %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[36:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_four")
reg.cof <- reg.cof[, .(tag, id, alpha_four)]

# match
syf <- reg.cof[syf, on = .(tag, id)]



################################################################################FF 5 factor
reg.roll <- list()
for (i in 36:168) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf + hml + smb + rmw + cma) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[36:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_five")
reg.cof <- reg.cof[, .(tag, id, alpha_five)]

# match
syf <- reg.cof[syf, on = .(tag, id)]


################################################################################FF 6 factor
reg.roll <- list()
for (i in 36:168) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf + hml + smb + rmw + cma + umd) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[36:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_six")
reg.cof <- reg.cof[, .(tag, id, alpha_six)]

# match
syf <- reg.cof[syf, on = .(tag, id)]


## ×Ümatch
alpha <- ret[, .(id, year, month, tag, month_return, TK)]
alpha <- syf[alpha, on = .(id, tag)
	][, .(id, year, month, TK, month_return, alpha_CAPM, alpha_three, alpha_four, alpha_five, alpha_six)]

save(alpha, file = "fundfactor.RData")
