load("monthlydata.RData")
data <- fund.TK

# add mkt rf
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, .(year, month, mkt, rf)]

data <- mkt[data, on = .(year, month)
	][, monthret_mkt := month_return - mkt
	][order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, monthret_mkt.1 := shift(monthret_mkt, n = 1, fill = NA, type = "lead"), keyby = .(id)]

# add four factor
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
fund.m <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)]

fund.m <- fund.m[, tag := seq(1:(.N)), keyby = id]

# four
ret <- four[fund.m, on = .(year, month)
	][, ret_rf := month_return - rf]

reg.roll <- list()
for (i in 36:168) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- coef(lm(ret_rf ~ mkt_rf + smb + hml + umd))[1] %>% as.list()
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[36:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

save(reg.cof, file = "fouralpha.RData")

ret <- reg.cof[ret, on = .(tag, id)
	]

setnames(ret, 3, "four_alpha")

alpha <- ret[, .(id, year, month, four_alpha)]

# match
data <- alpha[data, on = .(id, year, month)]

data <- data[!is.nan(TK)
	][, netflow := netflow / 100
	][, churn.rate := churn.rate / 100]

load("ability.RData")

data <- ability[data, on = .(id, year, sem)]


reg1 <- plm(TK ~ p.TK, data, model = "within", effect = "twoways", index = c("id", "date"))

reg2 <- plm(TK ~ p.TK + logfund_size + logfund_age , data, model = "within", effect = "twoways", index = c("id", "date"))

reg3 <- plm(TK ~ p.TK + logfund_size + logfund_age + churn.rate, data, model = "within", effect = "twoways", index = c("id", "date"))

reg4 <- plm(TK ~ p.TK + logfund_size + logfund_age + churn.rate + fundgap, data, model = "within", effect = "twoways", index = c("id", "date"))

reg5 <- plm(TK ~ p.TK + logfund_size + logfund_age + churn.rate + fundgap + picking + timing, data, model = "within", effect = "twoways", index = c("id", "date"))

stargazer(reg1, reg2, reg3, reg4, reg5, type = "html", out = "C:/Users/shenfan/Desktop/prospect value/new/table4.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes")))


