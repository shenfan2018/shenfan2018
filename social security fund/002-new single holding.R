# Social security fund performance better than mutual fund
# every fund 做
# 参考01-caculate holding return
# semi-annual holding return T+1
load("social_ret.RData")
social.ret <- social.ret[, .(ret.s.h.e = mean(ret.s)), keyby = .(year, sem, month)]

# fund
load("fund_ret.RData")
fund.ret <- fund.ret[, .(ret.f.h.e = mean(ret.f)), keyby = .(year, sem, month)]

# holding difference
hold.e <- fund.ret[social.ret, on = .(year, sem, month)]

##################################################
# value weighted
# fund
load("fundchar.RData")
fund.char <- fund.char[, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 6, 1, 2)
	][, .(id, year, sem, fund_size)]

# match
load("fund_ret.RData")
fund.ret <- fund.char[fund.ret, on = .(id, year, sem)
	][order(year, month)
	][!is.na(fund_size)
	][, market.proportion := fund_size / sum(fund_size), keyby = .(year, month)
	][, .(ret.f.h.v = sum(market.proportion * ret.f, na.rm = TRUE)), keyby = .(year, sem, month)]

# social
load("socialchar.RData")
social.char <- social.char[, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 6, 1, 2)
	][, .(fund, year, sem, social_size)]

# match
load("social_ret.RData")
social.ret <- social.char[social.ret, on = .(fund, year, sem)
	][order(year, month)
	][!is.na(social_size)
	][, market.proportion := social_size / sum(social_size), keyby = .(year, month)
	][, .(ret.s.h.v = sum(market.proportion * ret.s, na.rm = TRUE)), keyby = .(year, sem, month)]

# holding difference
hold.v <- fund.ret[social.ret, on = .(year, sem, month)]

###### added
hold <- hold.v[hold.e, on = .(year, sem, month)]

#################################################################
# mutual fund every (已经只剩下股票型和偏股型了）
# semi-annual holding return
# fund-NAV
load("fund-NAV.RData")
fund.ret <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(ret.f = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

load("fundchar.RData")
fund.char <- fund.char[, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 6, 1, 2)
	][, .(id, year, sem, fund_size)]

# match
fund.ret <- fund.char[fund.ret, on = .(id, year, sem)
	][order(year, month)
	][!is.na(fund_size)
	][, market.proportion := fund_size / sum(fund_size), keyby = .(year, month)
	][, .(ret.f.r.v = sum(market.proportion * ret.f, na.rm = TRUE), ret.f.r.e = mean(ret.f, na.rm = TRUE)), keyby = .(year, sem, month)]

# match
hold <- fund.ret[hold, on = .(year, sem, month)]

save(hold, file = "socialfundret.RData")

#t.test(hold[, "dif"])
#fit <- lm(dif ~ 1, hold)
#coeftest(fit, vcov. = NeweyWest)


################################################################
load("socialfundret.RData")
load("monthlyfactor.RData")
data <- monthlyfactor[hold, on = .(year, month)]

# 1. equal weighted
data <- data[, dif := ret.s.h.e - ret.f.h.e
	][, ret.s.h.e_rf := ret.s.h.e - rf
	][, ret.f.h.e_rf := ret.f.h.e - rf]

# 2. value weighted
data <- data[, dif := ret.s.h.v - ret.f.h.v
	][, ret.s.h.v_rf := ret.s.h.v - rf
	][, ret.f.h.v_rf := ret.f.h.v - rf]

t.test(data[bull == 0, dif])

# 回归
lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, data) %>% summary()

lm(dif ~ mkt_rf, data[bull == 1]) %>% summary()

a <- summary(lm(ret.s.h.e_rf ~ mkt_rf + smb + hml + umd, data[bull == 1]))$coef[1,3]


######## 试试
# 1. t test
a <- data[, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic)]


###############################################################
load("socialfundret.RData")
load("monthlyfactor.RData")

# weighted
hold <- hold[, .(year, month, ret.s.h.v, ret.f.h.v)]
data <- monthlyfactor[hold, on = .(year, month), nomatch = 0
	][, vol := ntile(vol, 10) # 
	][, vol := ifelse(vol == 10, 1, 0)]

data <- melt(data, id.vars = 1:16, measure.vars = 17:18)
data <- data[, value_rf := value - rf]

# 2. lm
a <- data[, .(raw_return = mean(value), raw_t = t.test(value)$statistic, alpha_capm = coef(lm(value_rf ~ mkt_rf))[1], t_capm = summary(lm(value_rf ~ mkt_rf))$coef[1, 3], alpha_three = coef(lm(value_rf ~ mkt_rf + smb + hml))[1], t_three = summary(lm(value_rf ~ mkt_rf + smb + hml))$coef[1, 3], alpha_four = coef(lm(value_rf ~ mkt_rf + smb + hml + umd))[1], t_four = summary(lm(value_rf ~ mkt_rf + smb + hml + umd))$coef[1, 3], alpha_five = coef(lm(value_rf ~ mkt_rf + smb + hml + rmw + cma))[1], t_five = summary(lm(value_rf ~ mkt_rf + smb + hml + rmw + cma))$coef[1, 3], alpha_six = coef(lm(value_rf ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t_six = summary(lm(value_rf ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[1, 3]), keyby = .(variable, vol)]

write.csv(a, "C://Users//shenfan//Desktop//mydata.csv")

# difference
load("socialfundret.RData")
load("monthlyfactor.RData")
hold <- hold[, .(year, month, ret.s.h.v, ret.f.h.v)]
data <- monthlyfactor[hold, on = .(year, month), nomatch = 0
	][, vol := ntile(vol, 10) # 
	][, vol := ifelse(vol == 10, 1, 0)
	][, dif := ret.s.h.v - ret.f.h.v]

a <- data[, .(raw_return = mean(dif), raw_t = t.test(dif)$statistic, alpha_capm = coef(lm(dif ~ mkt_rf))[1], t_capm = summary(lm(dif ~ mkt_rf))$coef[1, 3], alpha_three = coef(lm(dif ~ mkt_rf + smb + hml))[1], t_three = summary(lm(dif ~ mkt_rf + smb + hml))$coef[1, 3], alpha_four = coef(lm(dif ~ mkt_rf + smb + hml + umd))[1], t_four = summary(lm(dif ~ mkt_rf + smb + hml + umd))$coef[1, 3], alpha_five = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma))[1], t_five = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma))$coef[1, 3], alpha_six = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t_six = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[1, 3]), keyby = .(vol)]
