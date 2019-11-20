# performance persistence
load("fund_ret.RData")
load("fundchar.RData")

# 
fund.ret <- fund.ret[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))]
fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, date, year, quarter, fund_start, fund_size)]

fund.ret <- fund.char[fund.ret, on = .(id, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

fund.ret <- event[fund.ret, on = .(company)
	][, startyear := ifelse(is.na(startyear), 0, startyear)]

# 剔除2002和2004
fund.ret <- fund.ret[!startyear == 2002 & !startyear == 2004]

###################################################### 
# 3年,4年,5年
# 选择2006-2010， 2011-2015
data <- fund.ret[year > 2005 & year < 2016]

# 剔除2008之后成立的，无法对比
data <- data[, fund_start := as.Date(fund_start)
	][fund_start < as.Date("2005-12-31") #这个可以去选择
	][, proportion := fund_size / sum(fund_size), keyby = .(year, month, startyear)]

data <- data[, .(ret.vw = sum(proportion * ret.f), ret.ew = mean(ret.f)), keyby = .(year, month, startyear)]

# t test
before <- data[year > 2010]

a <- before[, .(mean.vw = mean(ret.vw), mean.eq = mean(ret.ew)), keyby = .(startyear)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

dif <- before[, ret.vw.low := shift(ret.vw, n = 1, fill = NA, type = "lag"), keyby = .(year, month)
	][, ret.ew.low := shift(ret.ew, n = 1, fill = NA, type = "lag"), keyby = .(year, month)
	][, dif.vw := ret.vw - ret.vw.low
	][, dif.ew := ret.ew - ret.ew.low
	][startyear == 2010
	][, .(dif.vw = t.test(dif.vw)$estimate, t = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 4], dif.ew = t.test(dif.ew)$estimate, t = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 4])]

write.csv(dif, "C://Users//shenfan//Desktop//mydatam.csv")


# factor analysis
before <- data[year > 2010]

four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")

before <- four[before, on = .(year, month)]

before <- before[, ret.ew_rf := ret.ew - rf
	][, ret.vw_rf := ret.vw - rf]

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

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, type = "html", out = "C:/Users/shenfan/Desktop/persistence2.doc", report = ('vc*t'))


########################## figure all difference
data <- fund.ret[year > 2005 & year < 2016]

# 剔除2008之后成立的，无法对比
data <- data[, fund_start := as.Date(fund_start)
	][fund_start < as.Date("2005-12-31") #这个可以去选择
	][, proportion := fund_size / sum(fund_size), keyby = .(year, month, startyear)]

data <- data[, .(ret.vw = sum(proportion * ret.f), ret.ew = mean(ret.f)), keyby = .(year, month, startyear)]

dif <- data[, ret.vw.low := shift(ret.vw, n = 1, fill = NA, type = "lag"), keyby = .(year, month)
	][, ret.ew.low := shift(ret.ew, n = 1, fill = NA, type = "lag"), keyby = .(year, month)
	][, dif.vw := ret.vw - ret.vw.low
	][, dif.ew := ret.ew - ret.ew.low
	][startyear == 2010]

dif <- melt(dif, id.vars = c("year", "month"), measure.vars = 8:9)
dif <- dif[, date := seq.Date(from = as.Date("2006/01/31", format = "%Y/%m/%d"), by = "month", length.out = 120)]
setnames(dif, "value", "difference")

xbreaks <- seq.Date(from = as.Date("2005/12/31", format = "%Y/%m/%d"), by = "year", length.out = 12)

ggplot(dif, aes(x = date)) +
	geom_line(aes(y = difference, colour = variable)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	geom_vline(aes(xintercept = as.Date("2010-12-31")), colour = 'black') +
	geom_vline(aes(xintercept = as.Date("2007-12-31")), colour = 'black', linetype = "dashed") +
	geom_vline(aes(xintercept = as.Date("2013-12-31")), colour = 'black', linetype = "dashed") +
	geom_hline(aes(yintercept = 0), colour = 'black') +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 8),
#	legend.title = element_blank(),
	legend.position = c(0.85, 0.90),
)


############################################################
# decompose
load("fund_ret.RData")
load("fundchar.RData")
load("social_ret.RData")
load("socialchar.RData")

# 
fund.ret <- fund.ret[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))]
fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, date, year, quarter, fund_start, fund_size)]

fund.ret <- fund.char[fund.ret, on = .(id, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

fund.ret <- event[fund.ret, on = .(company)
	][, startyear := ifelse(is.na(startyear), 0, startyear)]

# 剔除2002和2004
fund.ret <- fund.ret[!startyear == 2002 & !startyear == 2004]

###################################################### 
# 3年,4年,5年
# 选择2008-2010， 2011-2013
data <- fund.ret[year > 2007 & year < 2014]

MF <- data[startyear == 2010
	][as.Date(fund_start) < as.Date("2007-12-31")
	][, proportion := fund_size / sum(fund_size), keyby = .(year, month, company)]

MF <- MF[, .(ret.vw = sum(proportion * ret.f_mkt), ret.ew = mean(ret.f_mkt)), keyby = .(year, month, company)
	][, event := ifelse(year > 2010, 1, 0)]

# other fund
OMF <- data[startyear == 0
	][as.Date(fund_start) < as.Date("2007-12-31")
	][, proportion := fund_size / sum(fund_size), keyby = .(year, month)]

OMF <- OMF[, .(ret.vw = sum(proportion * ret.f_mkt), ret.ew = mean(ret.f_mkt)), keyby = .(year, month)
	][, event := ifelse(year > 2010, 1, 0)]

# match
MF <- OMF[MF, on = .(year, month)
	][, .(company, year, month, event, ret.ew, ret.vw, i.ret.ew, i.ret.vw)]

# mean
a <- MF[, lapply(.SD[, 3:6], mean), keyby = .(event, company)]

# dif
a <- MF[, dif.ew := i.ret.ew - ret.ew
	][, dif.vw := i.ret.vw - ret.vw
	][, .(dif.ew = t.test(dif.ew)$estimate, t = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 4], dif.vw = t.test(dif.vw)$estimate, t = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(event, company)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

############################################################
### social
load("social_ret.RData")
load("socialchar.RData")

social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(fund, social_company, date, year, quarter, social_size)]

social.ret <- social.char[social.ret, on = .(fund, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "social_company")

social.ret <- event[social.ret, on = .(social_company)
	][, startyear := ifelse(is.na(startyear), 0, startyear)]

# 剔除2002和2004
social.ret <- social.ret[!startyear == 2002 & !startyear == 2004]

###################################################### 
# 3年,4年,5年
# 选择2006-2010， 2011-2015
data <- social.ret[year > 2005 & year < 2016]

SF <- data[startyear == 2010
	][, proportion := social_size / sum(social_size), keyby = .(year, month, social_company)]

SF <-SF[, .(ret.vw = sum(proportion * ret.s_mkt), ret.ew = mean(ret.s_mkt)), keyby = .(year, month, social_company)
	][, event := ifelse(year > 2010, 1, 0)]

a <- SF[, lapply(.SD[, 3:4], mean), keyby = .(event, social_company)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")


############################################################## decompose all
# 用上面的

###################################################### 
# 3年,4年,5年
# 选择2008-2010， 2011-2013
data <- fund.ret[year > 2007 & year < 2014]

MF <- data[startyear == 2010
	][as.Date(fund_start) < as.Date("2007-12-31")
	][, proportion := fund_size / sum(fund_size), keyby = .(year, month, company)]

MF <- MF[, .(ret.vw = sum(proportion * ret.f_mkt), ret.ew = mean(ret.f_mkt)), keyby = .(year, month, company)
	][, event := ifelse(year > 2010, 1, 0)]

# other fund
OMF <- data[startyear == 0
	][as.Date(fund_start) < as.Date("2007-12-31")
	][, proportion := fund_size / sum(fund_size), keyby = .(year, month)]

OMF <- OMF[, .(ret.vw = sum(proportion * ret.f_mkt), ret.ew = mean(ret.f_mkt)), keyby = .(year, month)
	][, event := ifelse(year > 2010, 1, 0)]

# match
MF <- OMF[MF, on = .(year, month)
	][, .(company, year, month, event, ret.ew, ret.vw, i.ret.ew, i.ret.vw)]

# mean
a <- MF[, lapply(.SD[, 3:6], mean), keyby = .(event, company)]

# dif
a <- MF[, dif.ew := i.ret.ew - ret.ew
	][, dif.vw := i.ret.vw - ret.vw
	][, .(dif.ew = t.test(dif.ew)$estimate, t = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 4], dif.vw = t.test(dif.vw)$estimate, t = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(event, company)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")
