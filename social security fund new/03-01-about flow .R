# fund characterstic + performance
load("fundper.RData")
load("fund_ret.RData")
load("fundchar.RData")

# match
data <- fund.char
data <- data[, year := year(date)
	][, month := month(date)]

#data <- data[, lapply("log", str_c, colnames(data[, 7:10])) %>% unlist() := lapply(.SD[, 7:10], log)]

## match
fundper <- fundper[, c("ret.f.raw", "sem") := NULL]

data <- fundper[data, on = .(id, year, month)]

# add company char
load("companychar.RData")
company.char <- company.char[, .(company, date, logcompany_age, logcompany_size, company_flow)]

# match
data <- company.char[data, on = .(company, date)]

# add social company
# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event3.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

# match
data <- event[data, on = .(company, year, month)
	][, dummy := ifelse(is.na(dummy), 0, dummy)
	][, dummy := cumsum(dummy), keyby = .(company)]


## wins 现在的话fund_flow wins过的了
#library(DescTools)
#data <- data[, colnames(data[, 19:23]) := lapply(.SD[, 19:23], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# 滞后
data <- data[, fund_flow.1 := shift(fund_flow, n = 1, fill = NA, type = "lead"), keyby = .(id)]

write.csv(data, "C://Users//shenfan//Desktop//flowfundrelationship.csv")


##################################################################
### social security fund
load("socialper.RData")
load("socialchar.RData")

# match
data <- social.char
data <- data[, year := year(date)
	][, month := month(date)
	][, .(fund, social_company, social_style, social_style2, date, year, month, social_flow, logsocial_size, logsocial_age)]

#data <- data[, lapply("log", str_c, colnames(data[, 6:7])) %>% unlist() := lapply(.SD[, 6:7], log)]

# match
socialper <- socialper[, c("ret.s") := NULL]

data <- socialper[data, on = .(fund, year, month)]

## wins flow wins过了
#library(DescTools)
#data <- data[, colnames(data[, 10:12]) := lapply(.SD[, 10:12], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# add company char
load("companychar.RData")
company.char <- company.char[, .(company, date, logcompany_age, logcompany_size, company_flow)]
setnames(company.char, "company", "social_company")

# match
data <- company.char[data, on = .(social_company, date)]

# 滞后
data <- data[, social_flow.1 := shift(social_flow, n = 1, fill = NA, type = "lead"), keyby = .(fund)]

write.csv(data, "C://Users//shenfan//Desktop//flowsocialrelationship.csv")


##################################################################
### company
load("companyper.RData")
load("companychar.RData")

# match
data <- social.char
data <- data[, year := year(date)
	][, month := month(date)
	][, .(fund, social_company, social_style, social_style2, date, year, month, social_flow, logsocial_size, logsocial_age)]

#data <- data[, lapply("log", str_c, colnames(data[, 6:7])) %>% unlist() := lapply(.SD[, 6:7], log)]

# match
socialper <- socialper[, c("ret.s") := NULL]

data <- socialper[data, on = .(fund, year, month)]

## wins flow wins过了
#library(DescTools)
#data <- data[, colnames(data[, 10:12]) := lapply(.SD[, 10:12], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# add company char
load("companychar.RData")
company.char <- company.char[, .(company, date, logcompany_age, logcompany_size, company_flow)]
setnames(company.char, "company", "social_company")

# match
data <- company.char[data, on = .(social_company, date)]

# 滞后
data <- data[, social_flow.1 := shift(social_flow, n = 1, fill = NA, type = "lead"), keyby = .(fund)]

write.csv(data, "C://Users//shenfan//Desktop//flowsocialrelationship.csv")


###################################################################### company
load("company_ret.RData")
# to yearly
ret <- company.ret[, tag := seq(1:(.N)), keyby = .(company)]

reg.roll <- list()
for (i in 12:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 11 & tag <= i, {
		I <- list(ret.c.sub.12 = prod(ret.c.sub + 1) - 1, ret.c.all.12 = prod(ret.c.all + 1) - 1)
	},
	, keyby = .(company)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[12:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

company.ret <- reg.cof[company.ret, on = .(company, tag)]

a <- company.ret[, .(company, year, month, tag, ret.c.sub)]

# alpha
# match
four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")

a <- four[a, on = .(year, month)]

a <- a[, ret_rf := ret.c.sub - rf
	][!is.na(ret_rf)
	][, tag := 1:(.N), keyby = .(company)]

reg.roll <- list()
for (i in 36:max(a[, tag])) {
	reg.roll[[i]] <- a[tag >= i - 35 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf + hml + smb + umd) %>% coef() %>% as.list()
	},
	, keyby = .(company)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(a[, tag]), reg.roll)
roll <- roll[36:max(a[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

setnames(reg.cof, 3, "company.alpha.36")

a <- reg.cof[a, on = .(company, tag)
	][, .(company, year, month, company.alpha.36)]

# match
company.ret <- a[company.ret, on = .(company, year, month)]

# 取3，6，9，12月
company.ret <- company.ret[month == 3 | month == 6 | month == 9 | month == 12
	][, .(company, year, month, ret.c.sub.12, ret.c.all.12, company.alpha.36)]

load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/companychar.RData")
company.char <- company.char[, year := year(date)
	][, month := month(date)]

# match
data <- company.ret[company.char, on = .(company, year, month)]

# add mkt.12
ret <- four[, tag := seq(1:(.N))
	][, mkt := mkt_rf + rf]

syf <- vector()
for (i in 12:max(ret[, tag])) {
	syf[i] <- ret[tag >= i - 11 & tag <= i, {
		mkt.12 <- prod(mkt + 1) - 1
	},]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
setnames(syf, "syf", "mkt.12")
ret <- syf[ret, on = .(tag)
	][, .(year, month, mkt.12)]

# match
data <- ret[data, on = .(year, month)]

# add social dummy
# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event3.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")

# match
data <- event[data, on = .(company, year, month)
	][, dummy := ifelse(is.na(dummy), 0, dummy)
	][, dummy := cumsum(dummy), keyby = .(company)]

# company_code
company.code <- data[, .(company = unique(company))
	][, company_code := 1:.N]

# match
data <- company.code[data, on = .(company)]

# add star social fund
load("socialper.RData")
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/socialchar.RData")
social.char <- social.char[, .(company = unique(social_company)), keyby = .(fund)]
socialper <- social.char[socialper, on = .(fund)
	][, .(ret.s = mean(ret.s), alpha_four = mean(alpha_four)), keyby = .(company, year, month)
	][month == 3 | month == 6 | month == 9 | month == 12]

# match
data <- socialper[data, on = .(company, year, month)]

write.csv(data, "C://Users//shenfan//Desktop//companyflow.csv")