# 1991年开始
# month return
fund <- fread('C:/Users/shenfan/Desktop/prospect value/data US/chara.csv')
fund <- fund[, .(CRSP_FUNDNO, CALDT, mret_n)
	][, CALDT := substring(CALDT, 1, 6)]
setnames(fund, 1:3, c('id', 'Date', 'month_return'))

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

# match
alpha <- factor[fund, on = .(Date)
	][, ret_rf := month_return - RF
	][!is.na(ret_rf)
	][, tag := 1:(.N), keyby = .(id)]

ret <- alpha

# 滚动回归 CAPM
reg.roll <- list()
for (i in 36:456) { 
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ Mkt_RF) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:456, reg.roll)
roll <- roll[36:456]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_CAPM")

# syf
syf <- reg.cof[, .(tag, id, alpha_CAPM)]

#################################################################################FF 3 factor
reg.roll <- list()
for (i in 36:456) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ Mkt_RF + HML + SMB) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:456, reg.roll)
roll <- roll[36:456]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_three")
reg.cof <- reg.cof[, .(tag, id, alpha_three)]

# match
syf <- reg.cof[syf, on = .(tag, id)]

################################################################################carhart 4 factor
reg.roll <- list()
for (i in 36:456) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ Mkt_RF + HML + SMB + UMD) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:456, reg.roll)
roll <- roll[36:456]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_four")
reg.cof <- reg.cof[, .(tag, id, alpha_four)]

# match
syf <- reg.cof[syf, on = .(tag, id)]


################################################################################FF 5 factor
reg.roll <- list()
for (i in 36:456) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ Mkt_RF + HML + SMB + RMW + CMA) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:456, reg.roll)
roll <- roll[36:456]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_five")
reg.cof <- reg.cof[, .(tag, id, alpha_five)]

# match
syf <- reg.cof[syf, on = .(tag, id)]


################################################################################FF 6 factor
reg.roll <- list()
for (i in 36:456) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ Mkt_RF + HML + SMB + RMW + CMA + UMD) %>% coef() %>% as.list()
	},
	, keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:456, reg.roll)
roll <- roll[36:456]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "alpha_six")
reg.cof <- reg.cof[, .(tag, id, alpha_six)]

# match
syf <- reg.cof[syf, on = .(tag, id)]


## 总match
alpha <- ret[, .(id, Date, tag, month_return)]
alpha <- syf[alpha, on = .(id, tag)
	][, .(id, Date, month_return, alpha_CAPM, alpha_three, alpha_four, alpha_five, alpha_six)]

save(alpha, file = "USfundfactor.RData")


############################################################
# quarter return
fund <- fread('C:/Users/shenfan/Desktop/prospect value/data US/chara.csv')
fund <- fund[, .(CRSP_FUNDNO, CALDT, mret_n)
	][, CALDT := substring(CALDT, 1, 6)]
setnames(fund, 1:3, c('id', 'Date', 'month_return'))

fund <- fund[, year := substring(Date, 1, 4)
	][, month := as.numeric(substring(Date, 5, 6))
	][, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(quarter_return = prod(1 + month_return) - 1), keyby = .(id, year, quarter)]

write.csv(fund, "C://Users//shenfan//Desktop//USquarterreturn.csv")





