load("ability.RData")
data <- ability

########################################################################cumulative return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.s <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(sem_return = prod(AdjustedNAVGrowth2) - 1), keyby = .(id, year, sem)]

data <- data.s[data, on = .(id, year, sem)]

####################################################################### 6-factor alpha
ret <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)]

# 回归
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

ret <- four[ret, on = .(year, month)
	][, ret_rf := month_return - rf
	][, tag := seq(1:(.N)), keyby = .(id)]


reg.roll <- list()
for (i in 36:168) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf + smb + hml + umd + rmw + cma) %>% coef() %>% as.list()
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[36:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

ret <- reg.cof[ret, on = .(id, tag)]

setnames(ret, 3, "six_alpha")

ret <- ret[, .(id, year, month, six_alpha)]

alpha <- ret[, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][, .(six_alpha = mean(six_alpha, na.rm = TRUE)), keyby = .(id, year, sem)]

# match
data <- alpha[data, on = .(id, year, sem)]

# TK
load("monthlydata.RData")
fund.TK <- fund.TK[, .(TK = mean(TK)), keyby = .(id, year, sem)]

# match
data <- fund.TK[data, on = .(id, year, sem)]

save(data, file = "difmeasure.RData")

load("difmeasure.RData")

#### 进行下去
use <- data[!is.nan(six_alpha)
	][order(id, year, sem)
	][, six_alpha.1 := shift(six_alpha, n = 1, fill = NA, type = "lead"), keyby = .(id)]

use <- use[, lapply(colnames(use[, 4:9]), str_c, ".g") %>% unlist() := lapply(.SD[, 2:7], ntile, 3), keyby = .(year, sem)
	]
use = melt(use, id.vars = c("id", "year", "sem", "six_alpha.1"), measure.vars = colnames(use[, 11:16]))

rank <- use[, .(ret = mean(six_alpha.1, na.rm = TRUE)), keyby = .(year, sem, variable, value) 
	][!is.nan(ret)
	][!is.na(value)
	][, .(t = coeftest(lm(ret ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(ret)$estimate), keyby = .(variable, value)]

write.csv(rank, "C://Users//shenfan//Desktop//mydatam.csv")

# 做difference
SJ<- use[, .(ret = mean(six_alpha.1, na.rm = TRUE)), keyby = .(year, sem, variable, value)
	][!is.nan(ret)
	][!is.na(value)
	]
low <- SJ[value == 1]
high <- SJ[value == 3]
dif <- low[high, on = .(year, sem, variable)
	][, dif := i.ret - ret
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate), keyby = .(variable)]

write.csv(dif, "C://Users//shenfan//Desktop//mydatam2.csv")


# a <- rank[, .(ret = t.test(ret)$statistic, t = t.test(ret)$estimate), keyby = .(six_alpha.g)]

#a <- fread("C:/Users/shenfan/source/repos/Python-play/crawlerreport/ynhg.csv", encoding = "UTF-8")

#a <- a[, num := str_count(report, pattern = "中美贸易")]
