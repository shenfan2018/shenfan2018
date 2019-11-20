# mutual fund VS social fund
load("social_ret.RData")
load("fund_ret.RData")
load("socialchar.RData")
load("fundchar.RData")

# social
social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)]
social <- social.char[social.ret, on = .(fund, year, quarter)
	][, proportion := social_size / sum(social_size, na.rm = TRUE), keyby = .(year, month)
	][, .(ret.s.vw = sum(proportion * ret.s, na.rm = TRUE), ret.s.ew = mean(ret.s)), keyby = .(year, month)]

# mutual fund
fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)]
fund.ret <- fund.ret[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))]
fund <- fund.char[fund.ret, on = .(id, year, quarter)
	][, proportion := fund_size / sum(fund_size, na.rm = TRUE), keyby = .(year, month)
	][, .(ret.f.vw = sum(proportion * ret.f.raw, na.rm = TRUE), ret.f.ew = mean(ret.f.raw)), keyby = .(year, month)]

# VS
data <- social[fund, on = .(year, month), nomatch = 0]

#data <- melt(data, id.vars = c("year", "month"), measure.vars = 3:6)

# 四因子
four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

data <- four[data, on = .(year, month)]

# raw return
a <- data[, lapply(.SD[, 15:18], mean)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# dif
a <- data[, dif.ew := ret.s.ew - ret.f.ew
	][, dif.vw := ret.s.vw - ret.f.vw
	][, .(difference.ew = t.test(dif.ew)$estimate, t = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 4], difference.vw = t.test(dif.vw)$estimate, t = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 4])]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# factor analysis
#data <- data[, ret.f.ew_rf := ret.f.ew - rf
	#][, ret.f.vw_rf := ret.f.vw - rf
	#][, ret.s.ew_rf := ret.s.ew - rf
	#][, ret.s.vw_rf := ret.s.vw - rf]

data <- data[, colnames(data[, 15:18]) := .SD[, 15:18] - rf]

reg1 <- lm(ret.s.ew ~ mkt_rf + smb + hml + rmw + cma + umd, data)
reg2 <- lm(ret.f.ew ~ mkt_rf + smb + hml + rmw + cma + umd, data)
reg3 <- lm(dif.ew ~ mkt_rf + smb + hml + rmw + cma + umd, data)

reg4 <- lm(ret.s.vw ~ mkt_rf + smb + hml + rmw + cma + umd, data)
reg5 <- lm(ret.f.vw ~ mkt_rf + smb + hml + rmw + cma + umd, data)
reg6 <- lm(dif.vw ~ mkt_rf + smb + hml + rmw + cma + umd, data)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, type = "html", out = "C:/Users/shenfan/Desktop/社保/tables/new new/MFVSSF.doc", report = ('vc*t'))


# figure
data <- social[fund, on = .(year, month), nomatch = 0
	][, colnames(data[, 3:6]) := .SD[, 3:6] + 1
	][, colnames(data[, 3:6]) := lapply(.SD[, 3:6], cumprod)]

data <- data[, date := seq.Date(from = as.Date("2004/07/31", format = "%Y/%m/%d"), by = "month", length.out = 174)]
data <- melt(data, id.vars = c("date"), measure.vars = 3:6)

setnames(data, "value", "cum.ret")

#xbreaks <- c('2004-06-30', '2006-06-30', '2008-06-30', '2010-06-30', '2012-06-30', '2014-06-30', '2016-06-30', '2018-06-30')

xbreaks <- seq.Date(from = as.Date("2004/06/30", format = "%Y/%m/%d"), by = "year", length.out = 15)

ggplot(data, aes(x = date)) +
	geom_line(aes(y = cum.ret, linetype = variable)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 8),
#	legend.title = element_blank(),
	legend.position = c(0.05, 0.85),
)
