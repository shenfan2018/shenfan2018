# DID
load("fundalpha.Rdata")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, date, year, quarter, fund_start, fund_size)]

fund.alpha <- fund.char[fund.alpha, on = .(id, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

fund.alpha <- event[fund.alpha, on = .(company)
	][, startyear := ifelse(is.na(startyear), 0, startyear)]

# 剔除2002和2004
fund.alpha <- fund.alpha[!startyear == 2002 & !startyear == 2004]

###################################################### 
# 3年,4年,5年
# 选择2006-2010， 2011-2015
data <- fund.alpha[year > 2007 & year < 2014]

data <- melt(data, id.vars = c("id", "year", "month", "startyear", "fund_start", "fund_size"), measure.vars = 10:16)

# 剔除2008之后成立的，无法对比
data <- data[, fund_start := as.Date(fund_start)
	][fund_start < as.Date("2007-12-31") #这个可以去选择
	][!is.na(value)
	][, proportion := fund_size / sum(fund_size), keyby = .(year, month, startyear, variable)]

data <- data[, .(ret.vw = sum(proportion * value), ret.ew = mean(value)), keyby = .(year, month, startyear, variable)]

# t test
before <- data[year > 2010]

a <- before[, .(mean.vw = mean(ret.vw), mean.eq = mean(ret.ew)), keyby = .(startyear, variable)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

dif <- before[order(variable, year, month)
	][, ret.vw.low := shift(ret.vw, n = 1, fill = NA, type = "lag"), keyby = .(year, month, variable)
	][, ret.ew.low := shift(ret.ew, n = 1, fill = NA, type = "lag"), keyby = .(year, month, variable)
	][, dif.vw := ret.vw - ret.vw.low
	][, dif.ew := ret.ew - ret.ew.low
	][startyear == 2010
	][, .(dif.vw = t.test(dif.vw)$estimate, t = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 4], dif.ew = t.test(dif.ew)$estimate, t = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable)]

write.csv(dif, "C://Users//shenfan//Desktop//mydatam.csv")




########################################################################################################################## new DID
# DID
load("fundalpha.Rdata")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, date, year, quarter, fund_start, fund_size, fund_flow)]

fund.alpha <- fund.char[fund.alpha, on = .(id, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

fund.alpha <- event[fund.alpha, on = .(company)
	][, startyear := ifelse(is.na(startyear), 0, startyear)]

# 剔除2002和2004
fund.alpha <- fund.alpha[!startyear == 2002 & !startyear == 2004]

###################################################### 
# 3年,4年,5年
# 选择2006-2010， 2011-2015
data <- fund.alpha[year > 2007 & year < 2014]

data <- melt(data, id.vars = c("id", "year", "month", "startyear", "fund_start", "fund_size"), measure.vars = c("ret.f", "ret.f_mkt", "alpha_capm", "alpha_three", "alpha_four", "alpha_five", "alpha_six", "fund_flow"))

# 剔除2008之后成立的，无法对比
data <- data[, fund_start := as.Date(fund_start)
	][fund_start < as.Date("2007-12-31") #这个可以去选择
	][!is.na(value)
	][, proportion := fund_size / sum(fund_size), keyby = .(year, month, startyear, variable)]

data <- data[, .(ret.vw = sum(proportion * value), ret.ew = mean(value)), keyby = .(year, month, startyear, variable)
	][, event := ifelse(year > 2010, 1, 0)]

# delegated d
a <- data[, .(mean.vw = mean(ret.vw), mean.eq = mean(ret.ew)), keyby = .(event, startyear, variable)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference 
a <- data[order(event, year, month, startyear)
	][, ret.vw.low := shift(ret.vw, n = 8, fill = NA, type = "lag"), keyby = .(year, month)
	][, ret.ew.low := shift(ret.ew, n = 8, fill = NA, type = "lag"), keyby = .(year, month)
	][, dif.vw := ret.vw - ret.vw.low
	][, dif.ew := ret.ew - ret.ew.low
	][startyear == 2010
	][, .(dif.vw = t.test(dif.vw)$estimate, t = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.vw ~ 1), vcov. = NeweyWest)[, 4], dif.ew = t.test(dif.ew)$estimate, t = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif.ew ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(event, variable)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# DID
a <- data[order(event, year, month, startyear)
	][, ret.vw.low := shift(ret.vw, n = 8, fill = NA, type = "lag"), keyby = .(year, month)
	][, ret.ew.low := shift(ret.ew, n = 8, fill = NA, type = "lag"), keyby = .(year, month)
	][, dif.vw := ret.vw - ret.vw.low
	][, dif.ew := ret.ew - ret.ew.low
	][startyear == 2010]

t.test(a[event == 0 & variable == "fund_flow", dif.ew], a[event == 1 & variable == "fund_flow", dif.ew])



############################################################# reg
# DID
load("fundalpha.Rdata")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, date, year, quarter, fund_start, fund_size, fund_age)]

fund.alpha <- fund.char[fund.alpha, on = .(id, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

fund.alpha <- event[fund.alpha, on = .(company)
	][, startyear := ifelse(is.na(startyear), 0, startyear)]

# 剔除2002和2004
fund.alpha <- fund.alpha[!startyear == 2002 & !startyear == 2004]

###################################################### 
# 3年,4年,5年
# 选择2006-2010， 2011-2015
data <- fund.alpha[year > 2007 & year < 2014]

# 剔除2008之后成立的，无法对比
data <- data[, fund_start := as.Date(fund_start)
	][fund_start < as.Date("2007-12-31")
	][, event := ifelse(year > 2010, 1, 0)
	][, dummy := ifelse(startyear == 0, 0, 1)]

write.csv(data, "C://Users//shenfan//Desktop//DIDall.csv")

# all
# PSM
a <- data[, .(id, year, month, quarter, dummy, event, fund_size, fund_age)]

date <- as.data.table(seq.Date(from = as.Date("2008/01/01", format = "%Y/%m/%d"), by = "month", length.out = 72))
setnames(date, 1, "date")
date <- date[, year := year(date)
	][, month := month(date)
	][, tag := 1:.N]

a <- date[a, on = .(year, month), allow.cartesian = TRUE]

library(MatchIt)
roll <- list()
for (i in 1:max(a[, tag])) {
	roll[[i]] <- matchit(dummy ~ fund_size + fund_age, a[tag == i], method = "nearest", ratio = 1) %>% match.data() %>% as.data.table()
}
syf <- rbindlist(roll, fill = T)

syf.PSM <- syf[, .(id, year, month)]

data <- syf.PSM[data, on = .(id, year, month), nomatch = 0]

write.csv(data, "C://Users//shenfan//Desktop//DIDPSM.csv")



##################################################################################### 黄英的DID
# DID
load("fundalpha.Rdata")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, date, year, quarter, fund_start, fund_size, fund_age)]

fund.alpha <- fund.char[fund.alpha, on = .(id, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

fund.alpha <- event[fund.alpha, on = .(company)
	][, startyear := ifelse(is.na(startyear), 0, startyear)]

# 剔除2002和2004
fund.alpha <- fund.alpha[!startyear == 2002 & !startyear == 2004]

data <- fund.alpha[year > 2007 & year < 2014]

# 剔除2008之后成立的，无法对比
data <- data[, fund_start := as.Date(fund_start)
	][fund_start < as.Date("2007-12-31")
	][, event := ifelse(year > 2010, 1, 0)
	][, dummy := ifelse(startyear == 0, 0, 1)]

# all mean 前and后
D <- data[, lapply(.SD[, 6:16], mean, na.rm = TRUE), keyby = .(id, event, dummy, company)
	][, month := NULL]

b <- D[event == 0]

library(MatchIt)
roll <- matchit(dummy ~ fund_size + fund_age, b, method = "nearest", ratio = 1) %>% match.data() %>% as.data.table()
roll <- roll[, .(id)]

D <- roll[D, on = .(id), nomatch = 0]

# mean
a <- D[, lapply(.SD[, 5:12], mean), keyby = .(event, dummy)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference
dif <- D
dif <- dif[, colnames(dif[, 7:13]) := lapply(.SD[, 6:12], diff, difference = 1), keyby = .(id)
	][event == 1]

# D
dif <- melt(dif, id.vars = c("id", "dummy"), measure.vars = 7:14)

dif <- dif[, .(dif = t.test(value)$estimate, t = coeftest(lm(value ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(value~ 1), vcov. = NeweyWest)[, 4]), keyby = .(dummy, variable)]

write.csv(dif, "C://Users//shenfan//Desktop//mydatam.csv")

# DID
t.test(dif[dummy == 1, fund_flow], dif[dummy == 0, fund_flow], paired = FALSE)


################################################################### DID 回归
# DID
load("fundalpha.Rdata")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, date, year, quarter, fund_start, fund_size, fund_age, fund_style)]

fund.alpha <- fund.char[fund.alpha, on = .(id, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

fund.alpha <- event[fund.alpha, on = .(company)
	][, startyear := ifelse(is.na(startyear), 0, startyear)]

# 剔除2002和2004
fund.alpha <- fund.alpha[!startyear == 2002 & !startyear == 2004]

data <- fund.alpha[year > 2007 & year < 2014]

# 剔除2008之后成立的，无法对比
data <- data[, fund_start := as.Date(fund_start)
	][fund_start < as.Date("2007-12-31")
	][, event := ifelse(year > 2010, 1, 0)
	][, dummy := ifelse(startyear == 0, 0, 1)
	][, social := ifelse(event == 1 & dummy == 1, 1, 0)]

# PSM
a <- data[, .(id, year, month, quarter, dummy, event, fund_size, fund_age)]

date <- as.data.table(seq.Date(from = as.Date("2008/01/01", format = "%Y/%m/%d"), by = "month", length.out = 72))
setnames(date, 1, "date")
date <- date[, year := year(date)
	][, month := month(date)
	][, tag := 1:.N]

a <- date[a, on = .(year, month), allow.cartesian = TRUE]

library(MatchIt)
roll <- list()
for (i in 1:max(a[, tag])) {
	roll[[i]] <- matchit(dummy ~ fund_size + fund_age, a[tag == i], method = "nearest", ratio = 1) %>% match.data() %>% as.data.table()
}
syf <- rbindlist(roll, fill = T)

syf.PSM <- syf[, .(id, year, month)]

data <- syf.PSM[data, on = .(id, year, month), nomatch = 0]

data <- data[, lapply(colnames(data[, 12:18]), str_c, ".1") %>% unlist() := lapply(.SD[, 11:17], shift, n = 1, type = "lead", fill = NA), keyby = .(id)]

data <- data[, logfund_size := log(fund_size)
	][, logfund_age := log(fund_age)]

write.csv(data, "C://Users//shenfan//Desktop//DID.csv")


felm(ret.f_mkt.1 ~ ret.f_mkt * social + logfund_age + logfund_size | year + fund_style + id, data) %>% summary()

felm(alpha_four.1 ~ alpha_four * social + logfund_age + logfund_size | year + fund_style + id, data) %>% summary()