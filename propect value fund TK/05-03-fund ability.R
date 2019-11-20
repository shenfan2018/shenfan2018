# ability , fund gap, stock picking, timing

plm(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate + month_return + netflow, data, model = "within", effect = "twoways", index = c("id", "date")) %>% summary()


# 下面都是半年数据
########################## market timing
# 流通市值
MV <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//月个股流通市值//TRD_Mnth.xlsx")
MV <- as.data.table(MV)
setnames(MV, "Stkcd", "stock.id")
MV <- MV[month == 12 | month == 6
	][, sem := ifelse(month == 12, 2, 1)
	][, MV := Msmvosd * 1000
	][, .(stock.id, year, sem, MV)]

# 选择A股
A <- read_excel("C:/Users/shenfan/Desktop/wechat/Buffett alpha/A股.xlsx")
A <- as.data.table(A)
setnames(A, 1:2, c("code", "name"))
A <- A[, stock.id := substring(code, 1, 6)
	][, .(stock.id, name)]
MV <- A[MV, on = .(stock.id), nomatch = 0]

MV <- MV[, proportion.m := MV / sum(MV), keyby = .(year, sem)]

# fund portfolio的
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, year, sem, stock.id, MarketValue)]

# beta 过去12个月
# β 个股过去12个月超额收益率回归到市场的超额收益率
load("C:/Users/shenfan/source/repos/shenfan2018/Buffett alpha/stock20030101-20190331.RData")
# 所有数据应该变成month
stock.m <- stock
stock.m <- stock.m[order(stock.id, date)
	][, month := month(date)
	][, year := year(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_ret = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)]

# 加入mkt_ret
four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, .(year, month, mkt, rf)]

stock.m <- mkt[stock.m, on = .(year, month)]

stock.m <- stock.m[, mkt_rf := mkt - rf
	][, ret_rf := month_ret - rf]

stock.m <- stock.m[, tag := seq(1:(.N)), keyby = stock.id]

# ret <- stock.m[year < 2019]
ret <- stock.m

reg.roll <- list()
for (i in 12:195) {
	reg.roll[[i]] <- ret[tag >= i - 11 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf) %>% coef() %>% as.list()
	},
	keyby = .(stock.id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:195, reg.roll)
roll <- roll[12:195]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
ret <- reg.cof[ret, on = .(tag, stock.id)
	][, beta := mkt_rf
	][, .(stock.id, year, month, beta)]

beta <- ret[month == 6 | month == 12
	][, sem := ifelse(month == 6, 1, 2)]

# mkt
mktr <- mkt
mktr <- mktr[, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][, mkt2 := mkt + 1
	][, .(sem_mkt = prod(mkt2) - 1), keyby = .(year, sem)
	][, sem_mkt.1 := shift(sem_mkt, n = 1, fill = NA, type = "lead")]


# timing start
timing <- pf
timing <- MV[timing, on = .(stock.id, year, sem), nomatch = 0]
timing <- beta[timing, on = .(stock.id, year, sem), nomatch = 0]
timing <- mktr[timing, on = .(year, sem), nomatch = 0]

#这里删了
timing <- timing[!is.na(beta)
	][, proportion.s := MarketValue / sum(MarketValue), keyby = .(id, year, sem)]

timing <- timing[, .(timing = sum((proportion.s - proportion.m) * beta * sem_mkt.1)), keyby = .(id, year, sem)]


############### stock picking
load("C:/Users/shenfan/source/repos/shenfan2018/Buffett alpha/stock20030101-20190331.RData")
stock.s <- stock
stock.s <- stock.s[order(stock.id, date)
	][, quarter := quarter(date)
	][, year := year(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, Dretwd2 := Dretwd + 1
	][, .(sem_ret = prod(Dretwd2) - 1), keyby = .(stock.id, year, sem)
	][, sem_ret.1 := shift(sem_ret, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)]

picking <- pf
picking <- MV[picking, on = .(stock.id, year, sem), nomatch = 0]
picking <- beta[picking, on = .(stock.id, year, sem), nomatch = 0]
picking <- mktr[picking, on = .(year, sem), nomatch = 0]
picking <- stock.s[picking, on = .(stock.id, year, sem), nomatch = 0]

#这里删了
picking <- picking[!is.na(beta)
	][, proportion.s := MarketValue / sum(MarketValue), keyby = .(id, year, sem)]

picking <- picking[, .(picking = sum((proportion.s - proportion.m) * (sem_ret.1 - beta * sem_mkt.1), na.rm = TRUE)), keyby = .(id, year, sem)]


# fund gap?
# real
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.s <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(sem_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, sem)]

# holding return
#滞后
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, year, sem, stock.id, MarketValue)]
pf <- pf[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]

# stock.s 看上面
holdret <- stock.s[pf, on = .(stock.id, year, sem)
	][, proportion.s := MarketValue / sum(MarketValue), keyby = .(id, year, sem)
	][, .(holdret = sum(proportion.s * sem_ret, na.rm = TRUE)), keyby = .(id, year, sem)]

fund.gap <- holdret[data.s, on = .(id, year, sem)
	][, fundgap := sem_return - holdret
	][, .(id, year, sem, fundgap)]


# ability
ability <- timing
ability <- picking[ability, on = .(id, year, sem)]
ability <- fund.gap[ability, on = .(id, year, sem)]

save(ability, file = "ability.RData")