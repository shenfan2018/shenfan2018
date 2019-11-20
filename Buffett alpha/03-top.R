load("Bscore.RData")

load("stock20030101-20190331.RData")
stock.m <- stock
stock.m <- stock.m[, year := year(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_return = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)]

# mkt
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, .(year, month, mkt, rf)]

# date
date <- read_excel("C:/Users/shenfan/Desktop/Buffett alpha/date.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)]

# 选出每个月的top10
top <- Bscore
top <- top[order(year2, month, - Bscore.z)
	][, .SD[1:10], keyby = .(year2, month)
	][, .(year2, month, stock.id, Bscore.z)]
setnames(top, "year2", "year")

# 选出来的股滞后一期
top <- top[, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]
ret <- stock.m[top, on = .(stock.id, year, month)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month)
	][, ret2 := ret + 1
	][-(.N)]

 prod(ret[, ret2]) - 1

ret <- mkt[ret, on = .(year, month)
	][, mkt2 := mkt + 1
	][, tag := seq(1:(.N))]


# mktprod
syf <- vector()
for (i in 2:158) {
	syf[i] <- ret[tag >= 1 & tag <= i, {
		mktprod <- prod(mkt2)
	},]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

# retprod
sj <- vector()
for (i in 2:158) {
	sj[i] <- ret[tag >= 1 & tag <= i, {
		retprod <- prod(ret2)
	},]
}

sj <- as.data.table(sj)
sj <- sj[, tag := seq(1:(.N))]
ret <- sj[ret, on = .(tag)]

setnames(ret, c("sj", "syf"), c("retprod", "mktprod"))

ret <- ret[, retprod := ifelse(is.na(retprod), ret2, retprod)
	][, mktprod := ifelse(is.na(mktprod), mkt2, mktprod)]


# date
ret <- date[ret, on = .(year, month)]

# figure
xbreaks <- c('2006-06-30', '2008-06-30', '2010-06-30', '2012-06-30', '2014-06-30', '2016-06-30', '2018-06-30')

ret.fig <- ret[, .(date, mktprod, retprod)]
ret.fig = melt(ret.fig, id.vars = c("date"), measure.var = c("mktprod","retprod"))


ggplot(ret.fig, aes(x = date)) +
	geom_line(aes(y = value, linetype = variable)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 8),
#	legend.title = element_blank(),
	legend.position = c(0.15, 0.85),
)


# 超越市场
ret <- ret[, win := ifelse(ret > mkt, 1, 0)]
sum(ret[, win]) / ret[, .N]


write.csv(ret, "C://Users//shenfan//Desktop//gupiao.csv")


# ret <- mkt[ret, on = .(year, month)
#	][, .(year, month, ret, mkt)
#	][, win := ifelse(ret > mkt, 1, 0)]

# sum(ret[, win]) / 158

# prod(ret[, mkt2]) - 1

ret <- four[ret, on = .(year, month)
	][, rit := ret - rf]


reg1 <-lm(rit ~ mkt_rf , ret)  # %>% summary()
# (Intercept) 0.010091 0.002772 3.641 0.00037 ***

reg2 <- lm(rit ~ mkt_rf + smb + hml, ret) # %>% summary()

reg3 <- lm(rit ~ mkt_rf + smb + hml + umd, ret) # %>% summary()

reg4 <-lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, ret) # %>% summary()
# (Intercept)  0.009509   0.002659   3.577 0.000468 ***

stargazer(reg1, reg2, reg3,reg4, type = "html", out = "C:/Users/shenfan/Desktop/Buffett alpha/tables.doc")


# bear or bull
## bear/bull market (monthly)
# bull market when the month's past 12 month cumulative market risk premium is positive
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
bb <- four[, .(year, month, mkt_rf, rf)]

#先变月度
bb <- bb[, tag := seq(1:(.N))
	][, mkt_rf2 := mkt_rf + rf + 1
	][, rf2 := rf + 1]

reg.roll <- list()
for (i in 13:303) {
	reg.roll[[i]] <- bb[tag >= i - 12 & tag <= i - 1
	][, .(mkt = prod(mkt_rf2) - 1, rf = prod(rf2) - 1)]

	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:303, reg.roll)
roll <- roll[13:303]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
bb <- reg.cof[bb, on = .(tag)
	][, bull := ifelse(mkt > rf, 1, 0)
	][year > 2003
	][, .(year, month, bull)]

ret <- bb[ret, on = .(year, month)]

# bull
sum(ret[bull == 1, win]) / ret[bull == 1, .N]

# bear
sum(ret[bull == 0, win]) / ret[bull == 0, .N]



# 年化
(1 + (mean(ret[, ret]))) ^ 12 - 1

mean(ret[, ret]) * 12


# 胜率
top <- Bscore
top <- top[order(year2, month, - Bscore.z)
	][, .SD[1:10], keyby = .(year2, month)
	][, .(year2, month, stock.id, Bscore.z)]
setnames(top, "year2", "year")
# 选出来的股滞后一期
top <- top[, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]
ret <- stock.m[top, on = .(stock.id, year, month)]
ret <- ret[!is.na(month_return)
	][, win := ifelse(month_return > 0, 1, 0)
	][, .(per = sum(win) / (.N)), keyby = .(year, month)]
mean(ret[, per])

# 年
top <- Bscore
top <- top[order(year2, month, - Bscore.z)
	][, .SD[1:10], keyby = .(year2, month)
	][, .(year2, month, stock.id, Bscore.z)]
setnames(top, "year2", "year")
# 选出来的股滞后一期
top <- top[, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]
ret <- stock.m[top, on = .(stock.id, year, month)]
ret <- ret[!is.na(month_return)
	][, win := ifelse(month_return > 0, 1, 0)
	][, .(per = sum(win) / (.N)), keyby = .(year)]
mean(ret[, per])

# 年波动率
top <- Bscore
top <- top[order(year2, month, - Bscore.z)
	][, .SD[1:10], keyby = .(year2, month)
	][, .(year2, month, stock.id, Bscore.z)]
setnames(top, "year2", "year")
# 选出来的股滞后一期
top <- top[, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]
ret <- stock.m[top, on = .(stock.id, year, month)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month)
	][, ret2 := ret + 1
	][-(.N)]

sd(ret[,ret]) * (12^(1/2))


# sharpe ratio
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, .(year, month, mkt, rf)]
ret <- four[ret, on = .(year, month)]

# (((1 + mean(TK.g[TK.g == 5, rit])) ^ 12) - 1) / sd(TK.g[TK.g == 5, ret])

(((1 + mean(ret[, ret])) ^ 12) - (1 + mean(ret[, rf]))) / sd(ret[, ret])


# 最大回撤 drawdown = max （Di-Dj）/Di
top <- Bscore
top <- top[order(year2, month, - Bscore.z)
	][, .SD[1:10], keyby = .(year2, month)
	][, .(year2, month, stock.id, Bscore.z)]
setnames(top, "year2", "year")

# 选出来的股滞后一期
top <- top[, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]
ret <- stock.m[top, on = .(stock.id, year, month)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month)
	][, ret2 := ret + 1
	][-(.N)
	][, tag := seq(1:(.N))]

# retprod
syf <- vector()
for (i in 2:158) {
	syf[i] <- ret[tag >= 1 & tag <= i, {
		retprod <- prod(ret2)
	},]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

setnames(ret, c("syf"), c("retprod"))

ret <- ret[, retprod := ifelse(is.na(retprod), ret2, retprod)]

library(tseries)
maxdrawdown(ret[, retprod])










############################################################################### 调时间
load("SJBscore.RData")

load("stock20030101-20190331.RData")
stock.m <- stock
stock.m <- stock.m[, year := year(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_return = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)]

# mkt
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, mkt := mkt_rf + rf
	][, .(year, month, mkt, rf)]

# date
date <- read_excel("C:/Users/shenfan/Desktop/Buffett alpha/date.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)]

# 选出每个月的top10
top <- Bscore
top <- top[order(year2, month, - Bscore.z)
	][, .SD[1:10], keyby = .(year2, month)
	][, .(year2, month, stock.id, Bscore.z)]
setnames(top, "year2", "year")

# 选出来的股滞后一期
top <- top[, year := ifelse(month == 12, year + 1, year)
	][, month := ifelse(month == 12, 1, month + 1)]
ret <- stock.m[top, on = .(stock.id, year, month)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month)
	][, ret2 := ret + 1
	][-(.N)]

# date
ret <- date[ret, on = .(year, month)]

ret <- ret[date > as.Date("2017-02-01")]

ret <- mkt[ret, on = .(year, month)
	][, mkt2 := mkt + 1
	][, tag := seq(1:(.N))]


# mktprod
syf <- vector()
for (i in 2:158) {
	syf[i] <- ret[tag >= 1 & tag <= i, {
		mktprod <- prod(mkt2)
	},]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

# retprod
sj <- vector()
for (i in 2:158) {
	sj[i] <- ret[tag >= 1 & tag <= i, {
		retprod <- prod(ret2)
	},]
}

sj <- as.data.table(sj)
sj <- sj[, tag := seq(1:(.N))]
ret <- sj[ret, on = .(tag)]

setnames(ret, c("sj", "syf"), c("retprod", "mktprod"))

ret <- ret[, retprod := ifelse(is.na(retprod), ret2, retprod)
	][, mktprod := ifelse(is.na(mktprod), mkt2, mktprod)]


# figure
xbreaks <- c('2017-06-30', '2017-12-31', '2018-06-30','2018-12-31')

ret.fig <- ret[, .(date, mktprod, retprod)]
ret.fig = melt(ret.fig, id.vars = c("date"), measure.var = c("mktprod", "retprod"))


ggplot(ret.fig, aes(x = date)) +
	geom_line(aes(y = value, linetype = variable)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 8),
#	legend.title = element_blank(),
	legend.position = c(0.2, 0.15),
)

prod(ret[, ret2]) - 1

top <- date[top, on = .(year, month)]

