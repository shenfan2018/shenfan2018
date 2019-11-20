# shock, four
# 1. mobile 2014.12

# 2. EPU
EPU <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/China_Mainland_Paper_EPU.xlsx", sheet = "EPU 2000 onwards")
EPU <- as.data.table(EPU)
EPU <- EPU[, EPUcf := c(NA, diff(EPU, difference = 1))]
shock <- EPU

# 3. sentiment
sentiment <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/CICSI综合情绪指数表143955128/QX_CICSI.xlsx")
sentiment <- as.data.table(sentiment)
sentiment <- sentiment[order(year, month)
	][, sentcf := c(NA, diff(CICSI, difference = 1))]
# match
shock <- sentiment[shock, on = .(year, month)]

# 4. future 2015.6

##### PSM + DID
load("monthlydata.RData")
fund.TK <- fund.TK[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]

# shock
fund.TK <- fund.TK[, mobile := ifelse(date > as.Date("2014-12-31"), 1, 0)
	][, future := ifelse(date > as.Date("2015-6-30"), 1, 0)]
fund.TK <- shock[fund.TK, on = .(year, month)]

# TKmax的20%
data <- fund.TK
data <- data[!is.nan(TK)
	][!is.na(month_return.1)
	][, treat.g := ntile(TK, 5), keyby = .(year, month)
	][, treat.TK := ifelse(treat.g == 1, 1, 0)]
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

write.csv(reg.roll, "C://Users//shenfan//Desktop//PSM.csv")

# regression
plm(month_return.1 ~ treat.TK * future , logmodel = "within", effect = "twoways", index = c("id", "date")) %>% summary()

lm(month_return.1 ~ treat.TK + mobile + treat.TK * mobile, reg.roll) %>% summary()

plm(month_return.1 ~ treat.TK + mobile + treat.TK * mobile + logfund_size + logfund_age + churn.rate, reg.roll, model = "within", effect = "individual", index = c("id")) %>% summary()

plm(month_return.1 ~ treat.TK + mobile + treat.TK * mobile + logfund_size + logfund_age + churn.rate, reg.roll, model = "within", effect = "time", index = c("date")) %>% summary()




#plm(month_return.1 ~ treat.TK * future + logfund_size + logfund_age + churn.rate, reg.roll, model = "within", effect = "twoways", index = c("id", "date")) %>% summary()

#plm(month_return.1 ~ treat.TK * EPU + logfund_size + logfund_age + churn.rate, reg.roll, model = "within", effect = "twoways", index = c("id", "date")) %>% summary()

#plm(month_return.1 ~ treat.TK * CICSI + logfund_size + logfund_age + churn.rate, reg.roll, model = "within", effect = "twoways", index = c("id", "date")) %>% summary()



# b <- matchit(treat.TK ~ logfund_size + logfund_age + month_return + churn.rate + netflow, data, method = "nearest", ratio = 2) %>% match.data() %>% as.data.table()


############### 2019.9.21 add 
load("monthlydata.RData")
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
	][, TK.g := ntile(TK, 5), keyby = .(date)]

# 先做mobile
mobile <- data[date > as.Date("2011-12-31") & date < as.Date("2018-01-01")
	][, .(month_return.1 = mean(month_return.1)), keyby = .(date, TK.g)]

# 5-1
dif <- mobile[, month_return.1.1 := shift(month_return.1, n = 4, fill = NA, type = "lead"), keyby = .(date)
	][, dif := month_return.1.1 - month_return.1
	][!is.na(dif)
	][, mobile := ifelse(date > as.Date("2014-12-31"), 1, 0)
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(mobile)]

# 做future
future <- data[year > 2013 & year < 2017
	][, .(month_return.1 = mean(month_return)), keyby = .(date, TK.g)]

# 5-1
dif <- future[, month_return.1.1 := shift(month_return.1, n = 4, fill = NA, type = "lead"), keyby = .(date)
	][, dif := month_return.1.1 - month_return.1
	][!is.na(dif)
	][, future := ifelse(date > as.Date("2015-6-30"), 1, 0)
	][future == 1
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(future)]







