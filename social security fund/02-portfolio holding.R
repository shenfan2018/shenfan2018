## holding 是否超越大盘？？
time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")

# 季度
load("stock.RData")
stock.q <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, .SD[.N], keyby = .(stock.id, year, quarter)
	][, .(stock.id, year, quarter, Dsmvosd)]

#####社保
## 社保基金持股比例
IOS <- read_excel("C://Users//shenfan//Desktop//社保//data//社保基金持股比例.xlsx")
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IO.social", "DateQ"))

IOS <- IOS[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, quarter, IO.social)]
#	][, IO.social.2 := shift(IO.social, n = 1, fill = NA, type = "lag")
#	][, .(stock.id, year, quarter, IO.social, IO.social.2)]

IOS <- stock.q[IOS, on = .(stock.id, year, quarter)
	][, holds := IO.social * 0.01 * 1000 * Dsmvosd
	][order(year, quarter)
	][, proportion := holds / (sum(holds, na.rm = TRUE)), keyby = .(year, quarter)
	][order(stock.id, year, quarter)
	][, proportion.2 := shift(proportion, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)]

# 月度收益 
load("stock.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, month_ret := prod(Dretwd2) - 1, keyby = .(stock.id, year, month)
	][, vol := sd(Dretwd), keyby = .(stock.id, year, month)
	][, .SD[.N], keyby = .(stock.id, year, month)
	][, .(stock.id, year, quarter, month, month_ret)]

# t
# holding <- IOS[stock.m, on = .(stock.id, year, quarter), nomatch = 0
#	][!is.na(proportion)
#	][, .(ret = sum(proportion * month_ret)), keyby = .(year, month)]


## fund
IOF <- read_excel("C://Users//shenfan//Desktop//社保//data//基金持股比例.xlsx")
IOF <- as.data.table(IOF)
setnames(IOF, 1:58, c("code", "name", time))
IOF = melt(IOF, id.vars = c("code", "name"), measure.vars = time)
setnames(IOF, c("value", "variable"), c("IO.fund", "DateQ"))

IOF <- IOF[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, quarter, IO.fund)]
#	][, IO.social.2 := shift(IO.social, n = 1, fill = NA, type = "lag")
#	][, .(stock.id, year, quarter, IO.social, IO.social.2)]

IOF <- stock.q[IOF, on = .(stock.id, year, quarter)
	][, holds := IO.fund * 0.01 * 1000 * Dsmvosd
	][order(year, quarter)
	][, proportion := holds / (sum(holds, na.rm = TRUE)), keyby = .(year, quarter)
	][order(stock.id, year, quarter)
	][, proportion.2 := shift(proportion, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)]

# t
# holding <- IOF[stock.m, on = .(stock.id, year, quarter), nomatch = 0
#	][!is.na(proportion)
#	][, .(ret = sum(proportion * month_ret)), keyby = .(year, month)]


## social t+1
s.holding <- IOS[stock.m, on = .(stock.id, year, quarter), nomatch = 0
	][!is.na(proportion.2)
	][, .(ret.s = sum(proportion.2 * month_ret)), keyby = .(year, month)]

## fund t+1
f.holding<-IOF[stock.m, on = .(stock.id, year, quarter), nomatch = 0
	][!is.na(proportion.2)
	][, .(ret.f = sum(proportion.2 * month_ret)), keyby = .(year, month)]


## total.holding t+1
t.holding <- f.holding[s.holding, on = .(year, month)]

# 各种因子以及alpha
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

t.holding <- four[t.holding, on = .(year, month), nomatch = 0
	][, rit.s := ret.s - rf
	][, rit.f := ret.f - rf]

# 加入monthly volatility greater than top 10%/20%
# 各种因子以及alpha
four.d <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four.d <- as.data.table(four.d)
four.d <- four.d[, date := as.Date(trddy)
	][, year := year(date)
	][, month := month(date)
	][, mkt := mkt_rf + rf
	][, .(vol = sd(mkt)), keyby = .(year, month)
	][year < 2018 & year > 2003
	][-(1:3), .SD
	][, top.10 := ntile(vol, 10)
	][, top.10 := ifelse(top.10 == 10, 1, 0)
	][, top.5 := ntile(vol, 5)
	][, top.5 := ifelse(top.5 == 5, 1, 0)]

## bear/bull market (monthly)
# bull market when the month's past 12 month cumulative market risk premium is positive
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
bb <- four[, .(year, month, mkt_rf)]

#先变月度
bb <- bb[, tag := seq(1:(.N))
	][, mkt_rf2 := mkt_rf + 1]

reg.roll <- list()
for (i in 13:299) {
	reg.roll[[i]] <- bb[tag >= i - 12 & tag <= i - 1, {
		I <- prod(mkt_rf2, na.rm = TRUE) %>% as.list()
	}]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:299, reg.roll)
roll <- roll[13:299]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
bb <- reg.cof[bb, on = .(tag)
	][, bull := ifelse(V1 > 1, 1, 0)
	][year > 2003
	][, .(year, month, bull)]

# match
t.holding <- four.d[t.holding, on = .(year, month)]
t.holding <- bb[t.holding, on = .(year, month)]
t.holding <- t.holding[, dif := ret.s - ret.f]

# t test
# all sample
t.test(t.holding[, "ret.s"])
t.test(t.holding[, "ret.f"])
t.test(t.holding[, "dif"])
t.test(t.holding[, "ret.s"], t.holding[, "ret.f"], paired = FALSE)

# top.10
t.test(t.holding[top.10 == 1, "ret.s"])
t.test(t.holding[top.10 == 1, "ret.f"])
t.test(t.holding[top.10 == 1, "dif"])
t.test(t.holding[top.10 == 1, "ret.s"], t.holding[top.10 == 1, "ret.f"], paired = FALSE)

# bull
t.test(t.holding[bull == 0, "ret.s"])
t.test(t.holding[bull == 0, "ret.f"])
t.test(t.holding[bull == 0, "dif"])
t.test(t.holding[bull == 1, "ret.s"], t.holding[bull == 1, "ret.f"], paired = FALSE)

# regression
# all sample
lm(rit.s ~ mkt_rf + smb + hml + rmw + cma + umd, t.holding) %>% summary()

lm(rit.f ~ mkt_rf + smb + hml + rmw + cma + umd, t.holding) %>% summary()

lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, t.holding) %>% summary()


stargazer(f.h.t, s.h.t, f.h.t2, s.h.t2, type = "html", out = "C:/Users/shenfan/Desktop/社保/holding.doc")



############################################################## 新一轮
# 社保
load("IOS.RData")
# 调整到后一期股票收益
IOS.m <- IOS.m[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 1, 2, 1)]

# 月度收益 
load("stock.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, month_ret := prod(Dretwd2) - 1, keyby = .(stock.id, year, month)
	][, vol := sd(Dretwd), keyby = .(stock.id, year, month)
	][, .SD[.N], keyby = .(stock.id, year, month)
	][, .(stock.id, year, sem, month, month_ret)]

# 总社保的holding return
hold.s <- IOS.m[stock.m, on = .(stock.id, year, sem), nomatch = 0
	][, .(ret.s = sum(month_ret * proportion)), keyby = .(year, month)]

load("IOF.RData")
IOF.m <- IOF.m[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 1, 2, 1)]

# 总基金的holding return
hold.f <- IOF.m[stock.m, on = .(stock.id, year, sem), nomatch = 0
	][, .(ret.f = sum(month_ret * proportion)), keyby = .(year, month)]

hold.a <- hold.s[hold.f, on = .(year, month)
	][, dif := ret.s - ret.f]

t.test(hold.a[, "ret.f"])
t.test(hold.a[, "dif"])
fit <- lm(dif ~ 1, hold.a)
coeftest(fit, vcov. = NeweyWest)

################################################################# total sample 三四六因子
# raw return
t.test(hold.a[, ret.s])
t.test(hold.a[, ret.f])
t.test(hold.a[, dif])

# 各种因子以及alpha
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

hold.a <- four[hold.a, on = .(year, month), nomatch = 0
	][, rit.s := ret.s - rf
	][, rit.f := ret.f - rf
	][, dif := rit.s - rit.f]

# all sample
three.s <- lm(rit.s ~ mkt_rf + smb + hml , hold.a)
four.s <- lm(rit.s ~ mkt_rf + smb + hml + umd, hold.a)
six.s <- lm(rit.s ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a)

three.f <- lm(rit.f ~ mkt_rf + smb + hml, hold.a)
four.f <- lm(rit.f ~ mkt_rf + smb + hml + umd, hold.a)
six.f <- lm(rit.f ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a)

#three.d <- lm(dif ~ mkt_rf + smb + hml, hold.a)
#four.d <- lm(dif ~ mkt_rf + smb + hml + umd, hold.a)
#six.d <- lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a)

stargazer(three.s, four.s, six.s, three.f, four.f, six.f, type = "html", out = "C:/Users/shenfan/Desktop/社保/tables/new/holdingfactor2.doc")

##################################################### 目前到此

## bear/bull market (monthly)
# bull market when the month's past 12 month cumulative market risk premium is positive
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
bb <- four[, .(year, month, mkt_rf)]

#先变月度
bb <- bb[, tag := seq(1:(.N))
	][, mkt_rf2 := mkt_rf + 1]

reg.roll <- list()
for (i in 13:299) {
	reg.roll[[i]] <- bb[tag >= i - 12 & tag <= i - 1, {
		I <- prod(mkt_rf2, na.rm = TRUE) %>% as.list()
	}]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:299, reg.roll)
roll <- roll[13:299]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
bb <- reg.cof[bb, on = .(tag)
	][, bull := ifelse(V1 > 1, 1, 0)
	][year > 2003
	][, .(year, month, bull)]

# match
hold.a <- bb[hold.a, on = .(year, month)]

# 做dif
hold.a <- hold.a[, dif := ret.s - ret.f]

# bull
t.test(hold.a[bull == 1, "ret.s"])
t.test(hold.a[bull == 1, "ret.f"])
t.test(hold.a[bull == 1, "dif"])

bull.s <- lm(rit.s ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a[bull == 1])
bear.s <- lm(rit.s ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a[bull == 0])

bull.f <- lm(rit.f ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a[bull == 1])
bear.f <- lm(rit.f ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a[bull == 0])

stargazer(bull.s, bear.s, bull.f, bear.f, type = "html", out = "C:/Users/shenfan/Desktop/社保/tables/new/holdingbullbear.doc")


# 加入monthly volatility greater than top 10%/20%
# 各种因子以及alpha
four.d <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four.d <- as.data.table(four.d)
four.d <- four.d[, date := as.Date(trddy)
	][, year := year(date)
	][, month := month(date)
	][, mkt := mkt_rf + rf
	][, .(vol = sd(mkt)), keyby = .(year, month)
	][year < 2018 & year > 2003
	][-(1:3), .SD
	][, top.10 := ntile(vol, 10)
	][, top.10 := ifelse(top.10 == 10, 1, 0)
	][, top.5 := ntile(vol, 5)
	][, top.5 := ifelse(top.5 == 5, 1, 0)]
# match
hold.a <- four.d[hold.a, on = .(year, month)]


# top.10
t.test(hold.a[top.5 == 0, "ret.s"])
t.test(hold.a[top.5 == 0, "ret.f"])
t.test(hold.a[top.5 == 0, "dif"])


# regression
# 各种因子以及alpha
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

hold.a <- four[hold.a, on = .(year, month), nomatch = 0
	][, rit.s := ret.s - rf
	][, rit.f := ret.f - rf]

high.s <- lm(rit.s ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a[top.5 == 1])
low.s <- lm(rit.s ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a[top.5 == 0])

high.f <- lm(rit.f ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a[top.5 == 1])
low.f <- lm(rit.f ~ mkt_rf + smb + hml + rmw + cma + umd, hold.a[top.5 == 0])

stargazer(high.s, low.s, high.f, low.f, type = "html", out = "C:/Users/shenfan/Desktop/社保/tables/new/holdingvol.doc")


