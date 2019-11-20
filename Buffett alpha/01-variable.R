load("stock20030101-20190331.RData")

# 所有数据应该变成month
stock.m <- stock
stock.m <- stock.m[order(stock.id, date)
	][, month := month(date)
	][, year := year(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_ret = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)]

# 加入mkt_ret
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, .(year, month, mkt, rf)]

stock.m <- mkt[stock.m, on = .(year, month)]

stock.m <- stock.m[, mkt_rf := mkt - rf
	][, ret_rf := month_ret - rf]

# β 个股过去36个月超额收益率回归到市场的超额收益率
stock.m <- stock.m[, tag := seq(1:(.N)), keyby = stock.id]

# ret <- stock.m[year < 2019]
ret <- stock.m

reg.roll <- list()
for (i in 36:195) {
	reg.roll[[i]] <- ret[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf) %>% coef() %>% as.list()
	},
	keyby = .(stock.id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:195, reg.roll)
roll <- roll[36:195]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
ret <- reg.cof[ret, on = .(tag, stock.id)
	][, beta := mkt_rf
	][, .(stock.id, year, month, beta)]

# ret <- ret[, beta := shift(beta, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)]

beta <- ret[!is.na(beta)]

# match
variable <- beta


# IVOL 每个月的日超额收益率回归到Fama-French三因子模型后的残差项的标准差
stock.d <- stock

# 三因子 daily
three <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_daily/three_four_five_factor_daily/fivefactor_daily.csv")
three <- as.data.table(three)
three <- three[, date := as.Date(trddy)]

stock.d <- three[stock.d, on = .(date)]

# 残差
stock.d <- stock.d[, ret_rf := Dretwd - rf
	][, year := year(date)
	][, month := month(date)
	][, residuals := residuals(lm(ret_rf ~ mkt_rf + smb + hml)), keyby = .(stock.id, year, month)]

residuals <- stock.d[, .(IVOL = sd(residuals)), keyby = .(stock.id, year, month)]

# match
variable <- residuals[variable, on = .(stock.id, year, month)]

save(variable, file = "SJ.RData")

# BM: 年末公司股权的账面价值与相应年末股权市值之比
time3 <- c("2003-12-31", "2004-12-31", "2005-12-31", "2006-12-31", "2007-12-31", "2008-12-31", "2009-12-31", "2010-12-31", "2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31", "2018-12-31")
# 账面价值（所有者权益）
BM <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//股东权益合计 年报.xlsx")
BM <- as.data.table(BM)
setnames(BM, 1:18, c("code", "name", time3))
BM <- melt(BM, id.vars = c("code", "name"), measure.vars = time3)
setnames(BM, c("value", "variable"), c("equity", "date"))
BM <- BM[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, equity)
	][order(stock.id, year)]
#流通股市值 12月底
MV <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//月个股流通市值//TRD_Mnth.xlsx")
MV <- as.data.table(MV)
setnames(MV, "Stkcd", "stock.id")
MV <- MV[month == 12
	][, MV := Msmvosd * 1000
	][, .(stock.id, year, MV)]
BM <- MV[BM, on = .(stock.id, year)
	][, BM := equity / MV
	][, .(stock.id, year, BM)]

cheap <- BM

# ADR，RD分别是每年广告费用和研发费用与对应年末的股权市值之比。中国上市公司不单独披露，所以用销售费用和管理费用来代替广告费用和研发费用。
ADR <-read_excel("C://Users//shenfan//Desktop//Buffett alpha//销售费用.xlsx")
ADR <- as.data.table(ADR)
setnames(ADR, 1:18, c("code", "name", time3))
ADR <- melt(ADR, id.vars = c("code", "name"), measure.vars = time3)
setnames(ADR, c("value", "variable"), c("sale", "date"))
ADR <- ADR[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, sale)
	][order(stock.id, year)]

RD <- read_excel("C://Users//shenfan//Desktop//Buffett alpha//管理费用.xlsx")
RD <- as.data.table(RD)
setnames(RD, 1:18, c("code", "name", time3))
RD <- melt(RD, id.vars = c("code", "name"), measure.vars = time3)
setnames(RD, c("value", "variable"), c("manage", "date"))
RD <- RD[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, manage)
	][order(stock.id, year)]

DD <- RD[ADR, on = .(stock.id, year)]
DD <- MV[DD, on = .(stock.id, year)]

DD <- DD[, ADR := sale / MV
	][, RD := manage / MV
	][, .(stock.id, year, ADR, RD)]

cheap <- DD[cheap, on = .(stock.id, year)]


# GPOA: 年度毛利润与对应年末总资产账面价值比
GP <- read_excel("C://Users//shenfan//Desktop//Buffett alpha//毛利.xlsx")
GP <- as.data.table(GP)
setnames(GP, 1:18, c("code", "name", time3))
GP <- melt(GP, id.vars = c("code", "name"), measure.vars = time3)
setnames(GP, c("value", "variable"), c("grossprofit", "date"))
GP <- GP[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, grossprofit)
	][order(stock.id, year)]

# 总资产 (上一年的asset，所以year+1
ASSET<- read_excel("C://Users//shenfan//Desktop//Buffett alpha//总资产.xlsx")
ASSET <- as.data.table(ASSET)
setnames(ASSET, 1:18, c("code", "name", time3))
ASSET <- melt(ASSET, id.vars = c("code", "name"), measure.vars = time3)
setnames(ASSET, c("value", "variable"), c("totalasset", "date"))
ASSET <- ASSET[, year := year(date) + 1
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, totalasset)
	][order(stock.id, year)]

GPOA <- ASSET[GP, on = .(stock.id, year)
	][, GPOA := grossprofit / totalasset
	][, .(stock.id, year, GPOA)]

rm(GP)

# ACC: 年度净利润与年度经营性现金流的差
NP <- read_excel("C://Users//shenfan//Desktop//Buffett alpha//净利润.xlsx")
NP <- as.data.table(NP)
setnames(NP, 1:18, c("code", "name", time3))
NP <- melt(NP, id.vars = c("code", "name"), measure.vars = time3)
setnames(NP, c("value", "variable"), c("netprofit", "date"))
NP <- NP[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, netprofit)
	][order(stock.id, year)]

XJL <- read_excel("C://Users//shenfan//Desktop//Buffett alpha//经营性现金流.xlsx")
XJL <- as.data.table(XJL)
setnames(XJL, 1:18, c("code", "name", time3))
XJL <- melt(XJL, id.vars = c("code", "name"), measure.vars = time3)
setnames(XJL, c("value", "variable"), c("JYXJL", "date"))
XJL <- XJL[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, JYXJL)
	][order(stock.id, year)]

ACC <- XJL[NP, on = .(stock.id, year)]
ACC <- ASSET[ACC, on = .(stock.id, year)]
ACC <- ACC[, ACC := (netprofit - JYXJL) / totalasset
	][, .(stock.id, year, ACC)]

rm(NP, XJL)

# NOA: 每年经营性资产与经营性负债的差，并除以相应年初与年末的总资产平均值
# 用流动资产 流动负债
LDZX <- read_excel("C://Users//shenfan//Desktop//Buffett alpha//经营性//流动资产.xlsx")
LDZX <- as.data.table(LDZX)
setnames(LDZX, 1:18, c("code", "name", time3))
LDZX <- melt(LDZX, id.vars = c("code", "name"), measure.vars = time3)
setnames(LDZX, c("value", "variable"), c("LDZX", "date"))
LDZX <- LDZX[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, LDZX)
	][order(stock.id, year)]

LDFZ <- read_excel("C://Users//shenfan//Desktop//Buffett alpha//经营性//流动负债.xlsx")
LDFZ <- as.data.table(LDFZ)
setnames(LDFZ, 1:18, c("code", "name", time3))
LDFZ <- melt(LDFZ, id.vars = c("code", "name"), measure.vars = time3)
setnames(LDFZ, c("value", "variable"), c("LDFZ", "date"))
LDFZ <- LDFZ[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, LDFZ)
	][order(stock.id, year)]

NOA <- LDZX[LDFZ, on = .(stock.id, year)]
NOA <- ASSET[NOA, on = .(stock.id, year)]

NOA <- NOA[, NOA := (LDZX - LDFZ) / totalasset
	][, .(stock.id, year, NOA)]

Quality <- GPOA
Quality <- ACC[Quality, on = .(stock.id, year)]
Quality <- NOA[Quality, on = .(stock.id, year)]


##############################################################################
load("SJ.RData")
variable <- variable[, year2 := year
	][, year := ifelse(month < 5, year2 - 2, year2 - 1)]

variable <- cheap[variable, on = .(stock.id, year)]
variable <- Quality[variable, on = .(stock.id, year)]

save(variable, file = "SJ2.RData")
