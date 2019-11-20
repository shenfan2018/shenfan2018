load("addholdret.RData")
SJ <- SJ[, fung.gap := month_return - holding.ret]

## bear/bull market (monthly)
# bull market when the month's past 12 month cumulative market risk premium is positive
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
bb <- four[, .(year, month, mkt_rf, rf)]

#先变月度
bb <- bb[, tag := seq(1:(.N))
	][, mkt_rf2 := mkt_rf + rf + 1
	][, rf2 := rf + 1]

reg.roll <- list()
for (i in 13:299) {
	reg.roll[[i]] <- bb[tag >= i - 12 & tag <= i - 1
	][, .(mkt = prod(mkt_rf2) - 1, rf = prod(rf2) - 1)]

	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:299, reg.roll)
roll <- roll[13:299]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
bb <- reg.cof[bb, on = .(tag)
	][, bull := ifelse(mkt > rf, 1, 0)
	][year > 2003
	][, .(year, month, bull)]

data <- SJ

#################################################################### single sorting
## t期PV with t+1期return 一期为month
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, PW.g := ntile(PW, 5), keyby = .(date)]

TK.ret <- data[, .(ret = mean(month_return.1)), keyby = .(date, PW.g)
	][!is.na(ret)]
# 由于现在时间的ret是下一期的ret，所以时间要调整
TK.ret <- TK.ret[, year := year(date)
	][, month := month(date)
	][, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

#加入bb
TK.ret <- bb[TK.ret, on = .(year, month)]

################################################################
#一般运行到这里
low <- TK.ret[PW.g == 1
	][, .(date, year, month, ret)]
high <- TK.ret[PW.g == 5
	][, .(date, year, month, ret)]
dif <- low[high, on = .(date, year, month)
	][, dif := i.ret - ret]

t.test(TK.ret[PW.g == 1, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

############################ 加入因子
TK.g <- TK.ret
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
TK.g <- four[TK.g, on = .(year, month)]
TK.g <- TK.g[, rit := ret - rf]
dif <- four[dif, on = .(year, month)]

linear.1 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[PW.g == 1])
linear.2 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[PW.g == 2])
linear.3 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[PW.g == 3])
linear.4 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[PW.g == 4])
linear.5 <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, TK.g[PW.g == 5])
linear.dif <- lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, dif)

stargazer(linear.1, linear.2, linear.3, linear.4, linear.5, linear.dif, type = "html", out = "C:/Users/shenfan/Desktop/prospect value/tables/six factor.doc")

########################################## bear
bear <- TK.ret[bull == 1]

low <- bear[PW.g == 1
	][, .(date, year, month, ret)]
high <- bear[PW.g == 5
	][, .(date, year, month, ret)]
dif <- low[high, on = .(date, year, month)
	][, dif := i.ret - ret]

bear <- four[bear, on = .(year, month)
	][, rit := ret - rf]
dif <- four[dif, on = .(year, month)]

t.test(bear[PW.g == 2, ret])
t.test(dif[, dif])

lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, bear[PW.g == 1]) %>% summary()


fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)



######################################################################## holding return
load("addholdret.RData")
SJ <- SJ[, fung.gap := month_return - holding.ret]
data <- SJ

data <- data[order(id, date)
	][, month_return.1 := shift(holding.ret, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, PW.g := ntile(PW, 5), keyby = .(date)]

TK.ret <- data[, .(ret = mean(month_return.1)), keyby = .(date, PW.g)
	][!is.na(ret)]

# 由于现在时间的ret是下一期的ret，所以时间要调整
TK.ret <- TK.ret[, year := year(date)
	][, month := month(date)
	][, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]

#加入bb
TK.ret <- bb[TK.ret, on = .(year, month)]

################################################################
#一般运行到这里
low <- TK.ret[PW.g == 1
	][, .(date, year, month, ret)]
high <- TK.ret[PW.g == 5
	][, .(date, year, month, ret)]
dif <- low[high, on = .(date, year, month)
	][, dif := i.ret - ret]

t.test(TK.ret[PW.g == 1, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

########################################## bear
bear <- TK.ret[bull == 1]

low <- bear[PW.g == 1
	][, .(date, year, month, ret)]
high <- bear[PW.g == 5
	][, .(date, year, month, ret)]
dif <- low[high, on = .(date, year, month)
	][, dif := i.ret - ret]

t.test(bear[PW.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

# 加入因子
bear <- four[bear, on = .(year, month)
	][, rit := ret - rf]
dif <- four[dif, on = .(year, month)]







##############################################################################试试double sorting
# 先按TK，再按照fund gap
load("addholdret.RData")
SJ <- SJ[, fund.gap := month_return - holding.ret]
a <- SJ

a <- a[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, PW.g := ntile(PW, 5), keyby = .(date)
	][, fundgap.g := ntile(fund.gap, 5), keyby = .(date, PW.g)]

a <- a[, .(ret = mean(month_return.1)), keyby = .(date, PW.g, fundgap.g)
	][!is.na(ret)]

low <- a[PW.g == 1 & fundgap.g == 1]
high <- a[PW.g == 1 & fundgap.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(a[fundgap.g == 5, ret])

t.test(a[PW.g == 5 & fundgap.g == 1, ret])

t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)




########################################################################
# 分size

load("addholdret.RData")
SJ <- SJ[, fung.gap := month_return - holding.ret]
data <- SJ

#添加市值
marketvalue <- read_excel("C://Users//shenfan//Desktop//data//2//基金资产净值.xlsx")
marketvalue <- as.data.table(marketvalue)
setnames(marketvalue, 1:30, c("code", "name", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
marketvalue = melt(marketvalue, id.vars = c("code", "name"), measure.vars = c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(marketvalue, c("value", "variable"), c("fund_size", "DateQ"))
marketvalue <- marketvalue[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][, logfund_size := log(fund_size)
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][quarter == 2, sem := 1
	][quarter == 4, sem := 2
	][, id := substring(code, 1, 6)
	][, .(id, year, sem, logfund_size)
	][!is.na(logfund_size)]

# 加入
data <- marketvalue[data, on = .(id, year, sem), nomatch = 0]

data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, size.g := ntile(logfund_size, 2), keyby = .(date)
	][, PW.g := ntile(PW, 5), keyby = .(date, size.g)]

TK.ret <- data[, .(ret = mean(month_return.1)), keyby = .(date, size.g, PW.g)
	][!is.na(ret)]

# 选择小市值
TK.ret <- TK.ret[size.g == 2]

low <- TK.ret[PW.g == 1]
high <- TK.ret[PW.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.ret[PW.g == 1, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


##############################################################################分monthly vol
# 各种因子以及alpha
four.d <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four.d <- as.data.table(four.d)
four.d <- four.d[, date := as.Date(trddy)
	][, year := year(date)
	][, month := month(date)
	][, mkt := mkt_rf + rf
	][, .(vol = sd(mkt)), keyby = .(year, month)
	][year < 2018 & year > 2003
	][, vol.g := ntile(vol, 3)]

load("addholdret.RData")
data <- SJ

# 加入
data <- four.d[data, on = .(year, month), nomatch = 0]

data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][vol.g == 3   # 这里选择高波动率的
	][, PW.g := ntile(PW, 5), keyby = .(date)]

TK.ret <- data[, .(ret = mean(month_return.1)), keyby = .(date, PW.g)
	][!is.na(ret)]

low <- TK.ret[PW.g == 1]
high <- TK.ret[PW.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.ret[PW.g == 1, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


##############################################################################sentiment
sentiment <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/CICSI综合情绪指数表143955128/QX_CICSI.xlsx")
sentiment <- as.data.table(sentiment)
sentiment <- sentiment[, sent.g := ntile(CICSI, 5)]

load("addholdret.RData")
data <- SJ

# 加入
data <- sentiment[data, on = .(year, month), nomatch = 0]

data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][sent.g == 1 # sentiment
	][, TK.g := ntile(TK, 5), keyby = .(date)]

TK.ret <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

low <- TK.ret[TK.g == 1]
high <- TK.ret[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.ret[TK.g == 1, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

############################################################################
#double sorting 
# 先按fund gap，再按照TK
load("addholdret.RData")
SJ <- SJ[, fund.gap := month_return - holding.ret]
a <- SJ

a <- a[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, fundgap.g := ntile(fund.gap, 5), keyby = .(date)
	][, PW.g := ntile(PW, 5), keyby = .(date, fundgap.g)]

a <- a[, .(ret = mean(month_return.1)), keyby = .(date, PW.g, fundgap.g)
	][!is.na(ret)]

low <- a[PW.g == 1 & fundgap.g == 1]
high <- a[PW.g == 5 & fundgap.g == 1]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(a[fundgap.g == 5, ret])

t.test(a[PW.g == 5 & fundgap.g == 5, ret])

t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


##############################################################################
data <- SJ
data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK, 5), keyby = .(date)]

TK.ret <- data[, .(ret = mean(month_return.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

low <- TK.ret[TK.g == 1]
high <- TK.ret[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(dif[, dif])