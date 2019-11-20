##################################################################
# company performance
# monthly performance

# all sample or sub sample

# sub sample
load("fund-NAV.RData")
data.NAV <- data.NAV[, year := year(date)
	][, month := month(date)
	][, quarter := quarter(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(ret.f.raw = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter, month)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# match size
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/fundchar.RData")
fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, year, quarter, company, Category, category2, fund_size)]

data.sub <- fund.char[data.NAV, on = .(id, year, quarter), nomatch = 0
	][!is.na(fund_size)
	][, proportion := fund_size / sum(fund_size), keyby = .(company, year, month)
	][, .(ret.c.raw.sub = sum(proportion * ret.f.raw)), keyby = .(company, year, month)]


# all sample (data.NAV全的，需要size)
# 使用00-2-characterstic marketvalue
marketvalue <- marketvalue[, .(id, year, quarter, fund_size)]

# match company
fundcompany <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人2.xlsx")
fundcompany <- as.data.table(fundcompany)
setnames(fundcompany, 1:4, c("code", "name", "company.q", "company"))
fundcompany <- fundcompany[!is.na(name)
	][, id := substring(code, 1, 6)
	][, .(id, company)]

marketvalue <- fundcompany[marketvalue, on = .(id)]

data.all <- marketvalue[data.NAV, on = .(id, year, quarter), nomatch = 0
	][!is.na(fund_size)
	][, proportion := fund_size / sum(fund_size), keyby = .(company, year, month)
	][, .(ret.c.raw.all = sum(proportion * ret.f.raw)), keyby = .(company, year, month)]

# all
company.ret <- data.sub[data.all, on = .(company, year, month)]

save(company.ret, file = "company_ret.RData")


##################################################################
# 变成了monthly return有点问题
# attract more flow (company)
#load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/companychar.RData")
#company.char <- company.char[, year := year(date)
	#][, quarter := quarter(date)]
#load("company_ret.RData")
#company.ret <- company.ret[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))]

## match
#data <- company.char[company.ret, on = .(company, year, quarter)]

## add social dummy
## event
#event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event2.xlsx")
#event <- as.data.table(event)
#setnames(event, "fund.company", "company")

## match
#data <- event[data, on = .(company, year, month)
	#][, dummy := ifelse(is.na(dummy), 0, dummy)
	#][, dummy := cumsum(dummy), keyby = .(company)]

#write.csv(data, "C://Users//shenfan//Desktop//companyflow.csv")

###############################################################
load("company_ret.RData")
# to yearly
ret <- company.ret[, tag := seq(1:(.N)), keyby = .(company)]

reg.roll <- list()
for (i in 12:max(ret[, tag])) {
	reg.roll[[i]] <- ret[tag >= i - 11 & tag <= i, {
		I <- list(ret.c.raw.sub.12 = prod(ret.c.raw.sub + 1) - 1, ret.c.raw.all.12 = prod(ret.c.raw.all + 1) - 1)
	},
	, keyby = .(company)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:max(ret[, tag]), reg.roll)
roll <- roll[12:max(ret[, tag])]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

company.ret <- reg.cof[company.ret, on = .(company, tag)]

a <- company.ret[, .(company, year, month, tag, ret.c.raw.sub)]

# alpha
# match
four <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")

a <- four[a, on = .(year, month)]

a <- a[, ret_rf := ret.c.raw.sub - rf
	][!is.na(ret_rf)
	][, tag := 1:(.N), keyby = .(company)]

reg.roll <- list()
for (i in 36:max(a[,tag])) {
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
	][, .(company, year, month, ret.c.raw.sub.12, ret.c.raw.all.12, company.alpha.36)]

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

#data <- data[, company_flow.1 := shift(company_flow, n = 1, fill = NA, type = "lag"), keyby = .(company)]

#felm(company_flow.1 ~  company_flow + ret.c.raw.sub.12 + company_size + company_age | year + company, data) %>% summary()
