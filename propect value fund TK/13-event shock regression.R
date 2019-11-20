# TK分三组，matching
##### PSM + DID
load("monthlydata.RData")

# Winsorize
fund.TK <- fund.TK[, sem := NULL
	][, colnames(fund.TK[, 9:11]) := lapply(.SD[, 9:11], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]


fund.TK <- fund.TK[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]

# shock
fund.TK <- fund.TK[, mobile := ifelse(date > as.Date("2014-12-31"), 1, 0)
	][, future := ifelse(date > as.Date("2015-6-30"), 1, 0)
	][, holding := ifelse(date > as.Date("2015-08-08"), 1, 0)]

# TKmax的20%
data <- fund.TK
data <- data[!is.nan(TK)
	][!is.na(month_return.1)
	][, TK.g := ntile(TK, 10), keyby = .(year, month)
	][, TK.g2 := ifelse(TK.g == 1 | TK.g == 2, 1, ifelse(TK.g == 9 | TK.g == 10, 3, 2))
	][, treat.TK := ifelse(TK.g2 == 3, 1, 0)]

# PSM
data <- data[!is.na(netflow)
	][!is.na(churn.rate)
	][, c("p.TK", "p.PW", "p.LA", "p.CC", "quarter_return.1") := NULL
	][, tag := (year - 2008) * 12 + month]

roll <- list()
for (i in 1:109) {
	roll[[i]] <- matchit(treat.TK ~ logfund_size + logfund_age + month_return + churn.rate + netflow, data[tag == i + 11], method = "nearest", ratio = 1) %>% match.data() %>% as.data.table()
}
reg.roll <- rbindlist(roll, fill = T)

#write.csv(reg.roll, "C://Users//shenfan//Desktop//shockmax20%.csv")

# monthly fractional performance ranks ranging from zero to one are assigned to funds according to their return
reg.roll <- reg.roll[order(year, month, -month_return)
	][, pm := seq(1:.N), keyby = .(year, month)
	][, month_pm := pm / max(pm), keyby = .(year, month)
	][order(year, month, -month_return.1)
	][, pm := seq(1:.N), keyby = .(year, month)
	][, month_pm.1 := pm / max(pm), keyby = .(year, month)]


# mobile "2014-12-31"
felm(month_return.1 ~ treat.TK * mobile + month_return + logfund_size + logfund_age + churn.rate + netflow | id + year, reg.roll[date > as.Date("2012-12-31") & date < as.Date("2016-12-31")]) %>% summary()

plm(month_return.1 ~ treat.TK * mobile + month_return + logfund_size + logfund_age + churn.rate + netflow, reg.roll[date > as.Date("2013-12-31") & date < as.Date("2015-12-31")], effect = "twoways", model = "within", index = c("id", "date")) %>% summary()


# future "2015-6-30"
felm(month_return.1 ~ treat.TK * future + month_return + logfund_size + logfund_age + churn.rate + netflow | id + year, reg.roll[date > as.Date("2013-06-30") & date < as.Date("2017-06-30")]) %>% summary()

plm(month_return.1 ~ treat.TK * future + month_return + logfund_size + logfund_age + churn.rate + netflow, reg.roll[date > as.Date("2013-06-30") & date < as.Date("2017-06-30")], effect = "twoways", model = "within", index = c("id", "date")) %>% summary()


# holding "2015-08-08"
felm(month_return.1 ~ treat.TK * holding + month_return + logfund_size + logfund_age + churn.rate + netflow | id + year, reg.roll[date > as.Date("2014-08-07") & date < as.Date("2017-08-08")]) %>% summary() # treat.TK:holding . 

#plm(month_return.1 ~ treat.TK * holding + month_return + logfund_size + logfund_age + churn.rate + netflow, reg.roll[date > as.Date("2013-08-07") & date < as.Date("2017-08-08")], effect = "twoways", model = "within", index = c("id", "date")) %>% summary()

felm(month_pm.1 ~ treat.TK * holding + month_pm + logfund_size + logfund_age + churn.rate + netflow | id +
year, reg.roll[date > as.Date("2013-08-07") & date < as.Date("2017-08-08")]) %>% summary()



liner1 <- plm(month_return.1 ~ treat.TK * mobile + month_return + logfund_size + logfund_age + churn.rate + netflow, reg.roll[date > as.Date("2013-12-31") & date < as.Date("2015-12-31")], effect = "twoways", model = "within", index = c("id", "date"))

liner2 <- plm(month_return.1 ~ treat.TK * future + month_return + logfund_size + logfund_age + churn.rate + netflow, reg.roll[date > as.Date("2014-06-30") & date < as.Date("2016-06-30")], effect = "twoways", model = "within", index = c("id", "date"))

liner3 <- plm(month_return.1 ~ treat.TK * holding + month_return + logfund_size + logfund_age + churn.rate + netflow, reg.roll[date > as.Date("2014-08-07") & date < as.Date("2016-08-08")], effect = "twoways", model = "within", index = c("id", "date"))

# 写出
stargazer(liner1, liner2, liner3, type = "html", out = "C://Users//shenfan//Desktop//prospect value//tables//plm1year.doc", add.lines = list(c("fund", "yes", "yes", "yes"), c("year", "yes", "yes", "yes")), report = ('vc*t'))




##################################################################
# no match
load("monthlydata.RData")

# Winsorize
fund.TK <- fund.TK[, sem := NULL
	][, colnames(fund.TK[, 9:11]) := lapply(.SD[, 9:11], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]


fund.TK <- fund.TK[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]

# shock
fund.TK <- fund.TK[, mobile := ifelse(date > as.Date("2014-12-31"), 1, 0)
	][, future := ifelse(date > as.Date("2015-6-30"), 1, 0)
	][, holding := ifelse(date > as.Date("2015-08-08"), 1, 0)]

# TKmax的20%
data <- fund.TK
data <- data[!is.nan(TK)
	][!is.na(month_return.1)
	][, TK.g := ntile(TK, 10), keyby = .(year, month)
	][, TK.g2 := ifelse(TK.g == 1 | TK.g == 2, 1, ifelse(TK.g == 9 | TK.g == 10, 3, 2))
	][, treat.TK := ifelse(TK.g2 == 3, 1, 0)]

data <- data[order(year, month,  month_return)
	][, pm := seq(1:.N), keyby = .(year, month)
	][, month_pm := pm / max(pm), keyby = .(year, month)
	][order(year, month,  month_return.1)
	][, pm := seq(1:.N), keyby = .(year, month)
	][, month_pm.1 := pm / max(pm), keyby = .(year, month)]


# 
#liner1 <- felm(month_pm.1 ~ treat.TK * mobile + month_pm + logfund_size + logfund_age + churn.rate + netflow | id + year, data[date > as.Date("2012-12-31") & date < as.Date("2016-12-31")])

#liner2 <- felm(month_pm.1 ~ treat.TK * future + month_pm + logfund_size + logfund_age + churn.rate + netflow | id + year, data[date > as.Date("2013-06-30") & date < as.Date("2017-06-30")])

#liner3 <- felm(month_pm.1 ~ treat.TK * holding + month_pm + logfund_size + logfund_age + churn.rate + netflow | id +
#year, data[date > as.Date("2013-08-07") & date < as.Date("2017-08-08")])

liner1 <- plm(month_pm.1 ~ treat.TK * mobile + month_pm + logfund_size + logfund_age + churn.rate + netflow, data[date > as.Date("2012-12-31") & date < as.Date("2016-12-31")], effect = "twoways", model = "within", index = c("id", "date"))

liner2 <- plm(month_pm.1 ~ treat.TK * future + month_pm + logfund_size + logfund_age + churn.rate + netflow, data[date > as.Date("2013-06-30") & date < as.Date("2017-06-30")], effect = "twoways", model = "within", index = c("id", "date"))

liner3 <- plm(month_pm.1 ~ treat.TK * holding + month_pm + logfund_size + logfund_age + churn.rate + netflow, data[date > as.Date("2013-08-07") & date < as.Date("2017-08-08")], effect = "twoways", model = "within", index = c("id", "date"))


# 写出
stargazer(liner1, liner2, liner3, type = "html", out = "C://Users//shenfan//Desktop//prospect value//tables//nomatchtreatplm2yearpm.doc", add.lines = list(c("fund", "yes", "yes", "yes"), c("year", "yes", "yes", "yes")), report = ('vc*t'))



########################################
load("monthlydata.RData")

# Winsorize
fund.TK <- fund.TK[, sem := NULL
	][, colnames(fund.TK[, 9:11]) := lapply(.SD[, 9:11], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]


fund.TK <- fund.TK[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]

# TKmax的20%
data <- fund.TK
data <- data[!is.nan(TK)
	][!is.na(month_return.1)
	][, TK.g := ntile(TK, 5), keyby = .(date)]

data <- fund.TK[order(year, month, month_return)
	][, pm := seq(1:.N), keyby = .(year, month)
	][, month_pm := pm / max(pm), keyby = .(year, month)
	][order(year, month, month_return.1)
	][, pm := seq(1:.N), keyby = .(year, month)
	][, month_pm.1 := pm / max(pm), keyby = .(year, month)]


felm(month_pm.1 ~ TK + month_pm + logfund_size + logfund_age + churn.rate + netflow | year + id, data) %>% summary()

plm(month_return.1 ~ TK + month_return + logfund_size + logfund_age + churn.rate + netflow, data, effect = "twoways", model = "within", index = c("id", "date")) %>% summary()



lm(month_return.1 ~ TK + month_return + logfund_size + logfund_age + churn.rate + netflow, data) %>% summary()

felm(month_return.1 ~ TK | id, fund.TK) %>% summary()

# fama macbeth pmg

#fund.TK <- fund.TK[, mobile := ifelse(date > as.Date("2014-12-31"), 1, 0)
	#][, future := ifelse(date > as.Date("2015-6-30"), 1, 0)
	#][, holding := ifelse(date > as.Date("2015-08-08"), 1, 0)]

pmg(month_return.1 ~ TK + month_return + logfund_size + logfund_age + churn.rate + netflow, data[date > as.Date("2015-08-08") & date < as.Date("2018-08-08")], index = c("date", "id")) %>% summary()


############################### plot
load("monthlydata.RData")

# Winsorize
fund.TK <- fund.TK[, sem := NULL
	][, colnames(fund.TK[, 9:11]) := lapply(.SD[, 9:11], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]


fund.TK <- fund.TK[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]

# TKmax的20%
data <- fund.TK
data <- data[!is.nan(TK)
	][!is.na(month_return.1)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, .(ret = mean(month_return.1)), keyby = .(date, TK.g)]

# dif
dif <- data[, ret.1 := shift(ret, n = 4, fill = NA, type = 'lead'), keyby = .(date)
	][, dif := ret.1 - ret
	][!is.na(dif)]

ggplot(dif, aes(x = date)) +
	geom_line(aes(y = dif)) +
	theme_bw() +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 8),
#	legend.title = element_blank(),
	legend.position = c(0.15, 0.85),
)

