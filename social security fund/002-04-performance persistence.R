# performance persistence
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/fund_ret.RData")
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/fundchar.RData")

# 
fund.ret <- fund.ret[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))]
fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, date, year, quarter, fund_start, fund_size)]

data <- fund.char[fund.ret, on = .(id, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

data <- event[data, on = .(company)
	][, startyear := ifelse(is.na(startyear), 0, startyear)]

# 剔除2002和2004
data <- data[!startyear == 2002 & !startyear == 2004]

## 3年
# 选择2008-2010， 2011-2013
data <- data[year > 2007 & year < 2014]

# 剔除2008之后成立的，无法对比
data <- data[fund_start < as.Date("2007-12-31")
	][, proportion := fund_size / sum(fund_size), keyby = .(year, month, startyear)]

data <- data[, .(ret.vw = sum(proportion * ret.f.raw), ret.ew = mean(ret.f.raw)), keyby = .(year, month, startyear)]

# t test
before <- data[year > 2010]

a <- before[, .(mean.vw = mean(ret.vw), mean.eq = mean(ret.ew)), keyby = .(startyear)]

dif <- before[, ret.vw.low := shift(ret.vw, n = 1, fill = NA, type = "lag"), keyby = .(year, month)
	][, ret.ew.low := shift(ret.ew, n = 1, fill = NA, type = "lag"), keyby = .(year, month)
	][, dif.vw := ret.vw - ret.vw.low
	][, dif.ew := ret.ew - ret.ew.low
	][startyear == 2010
	][, .(dif.vw = t.test(dif.vw)$estimate, t = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 4], dif.ew = t.test(dif.ew)$estimate, t = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 4])]

write.csv(dif, "C://Users//shenfan//Desktop//mydatam.csv")

# factor analysis
four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")

data <- four[data, on = .(year, month)]

data <- data[, ret.ew_rf := ret.ew - rf
	][, ret.vw_rf := ret.vw - rf]

before <- data[year > 2010]

dif <- before[, ret.vw.low := shift(ret.vw, n = 1, fill = NA, type = "lag"), keyby = .(year, month)
	][, ret.ew.low := shift(ret.ew, n = 1, fill = NA, type = "lag"), keyby = .(year, month)
	][, dif.vw := ret.vw - ret.vw.low
	][, dif.ew := ret.ew - ret.ew.low
	][startyear == 2010]


reg1 <- lm(ret.ew_rf ~ mkt_rf + smb + hml + rmw + cma + umd, before[startyear == 2010])


reg1 <- lm(ret.ew_rf ~ mkt_rf + smb + hml + rmw + cma + umd, before[startyear == 2010])
reg2 <- lm(ret.ew_rf ~ mkt_rf + smb + hml + rmw + cma + umd, before[startyear == 0])
reg3 <- lm(dif.ew ~ mkt_rf + smb + hml + rmw + cma + umd, dif)

reg4 <- lm(ret.vw_rf ~ mkt_rf + smb + hml + rmw + cma + umd, before[startyear == 2010])
reg5 <- lm(ret.vw_rf ~ mkt_rf + smb + hml + rmw + cma + umd, before[startyear == 0])
reg6 <- lm(dif.vw ~ mkt_rf + smb + hml + rmw + cma + umd, dif)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, type = "html", out = "C:/Users/shenfan/Desktop/社保/tables/new new/persistence2.doc", report = ('vc*t'))


################################################################# 904
