############################################################# new relative big
load("fund_ret.RData")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, fund_category := NULL]

fund <- fund.char[fund.ret, on = .(id, year, quarter)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event22.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")
event <- event[, socialstart := as.Date(socialstart)]

fund <- event[fund, on = .(company)
	][, dummy := ifelse(as.Date(date) > as.Date(socialstart), 1, 0)
	][, dummy := ifelse(is.na(dummy), 0, dummy)]

# 删除company里面基金少于4个
fund <- fund[, N := (.N), keyby = .(year, month, company)
	][N > 3]

# relative big
fund <- fund[, relativebig := ntile(fund_size, 2), keyby = .(year, month, company)
	][, relativebig := ifelse(relativebig == 4, 1, 0)]

fund <- fund[order(id, year, month)
	][, .(id, company, date, year, month, quarter, fund_style, fund_size, fund_age, ret.f, ret.f_mkt, dummy, relativebig)]

setnames(fund, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# social
load("social_ret.RData")
load("socialchar.RData")
social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)]

social <- social.char[social.ret, on = .(fund, year, quarter)]
# social id 
socialid <- social[order(fund)
	][, .SD[1], keyby = .(fund)
	][, id := 900001:900044
	][, .(fund, id)]

# match
social <- socialid[social, on = .(fund)
	][, .(id, social_company, date, year, month, quarter, social_style2, social_size, social_age, ret.s, ret.s_mkt)
	][, dummy := 0
	][, relativebig := 0
	][, social_size := social_size * 10000]


setnames(social, 2, c("company"))
setnames(social, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# match rbindlist
data <- rbindlist(list(fund, social))

# company
load("companychar.RData")
data <- company.char[data, on = .(company, date)]

data <- data[order(id, year, month)
	][, ret.1 := shift(ret, n = 12, fill = NA, type = "lead"), keyby = .(id)
	][, ret_mkt.1 := shift(ret_mkt, n = 12, fill = NA, type = "lead"), keyby = .(id)
	][, log_size := log(size)
	][, log_age := log(age)]

felm(ret_mkt.1 ~ relativebig + dummy + relativebig * dummy + ret_mkt + log_size + log_age + logcompany_size + logcompany_age | year + style + id, data) %>% summary()

write.csv(data, "C://Users//shenfan//Desktop//relativebigprovision.csv")

################################################################################## relatively small
load("fund_ret.RData")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, fund_category := NULL]

fund <- fund.char[fund.ret, on = .(id, year, quarter)]


# 删除company里面基金少于4个
fund <- fund[, N := (.N), keyby = .(year, month, company)
	][N > 3]

# relative big
fund <- fund[, relativesmall := ntile(fund_size, 2), keyby = .(year, month, company)
	][, relativesmall := ifelse(relativesmall == 1, 1, 0)]

# dummy均为0
fund <- fund[, dummy := 0]

fund <- fund[order(id, year, month)
	][, .(id, company, date, year, month, quarter, fund_style, fund_size, fund_age, ret.f, ret.f_mkt, dummy, relativesmall)]

setnames(fund, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# social
load("social_ret.RData")
load("socialchar.RData")
social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)]

social <- social.char[social.ret, on = .(fund, year, quarter)]
# social id 
socialid <- social[order(fund)
	][, .SD[1], keyby = .(fund)
	][, id := 900001:900044
	][, .(fund, id)]

# match socialsize*100
social <- socialid[social, on = .(fund)
	][, .(id, social_company, date, year, month, quarter, social_style2, social_size, social_age, ret.s, ret.s_mkt)
	][, dummy := 1
	][, realtivesmall := 1
	][, social_size := social_size * 10000]


setnames(social, 2, c("company"))
setnames(social, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# match rbindlist
data <- rbindlist(list(fund, social))

# company
load("companychar.RData")
data <- company.char[data, on = .(company, date)]

data <- data[order(id, year, month)
	][, ret.1 := shift(ret, n = 1, fill = NA, type = "lead"), keyby = .(id)]

write.csv(data, "C://Users//shenfan//Desktop//relativesmallreceipt.csv")


############################################################################################################################# delete top25%的fund
load("fund_ret.RData")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, fund_category := NULL]

fund <- fund.char[fund.ret, on = .(id, year, quarter)]

# 删除company里面基金少于4个
fund <- fund[, N := (.N), keyby = .(year, month, company)
	][N > 3]

# relative big
fund <- fund[, relativesmall := ntile(fund_size, 2), keyby = .(year, month, company)
	][, relativesmall := ifelse(relativesmall == 1, 1, 0)]

# delete top25% fund
fund <- fund[, rank := ntile(ret.f, 4), keyby = .(year, month, company)
	][!rank == 4]

# dummy均为0
fund <- fund[, dummy := 0]

fund <- fund[order(id, year, month)
	][, .(id, company, date, year, month, quarter, fund_style, fund_size, fund_age, ret.f, ret.f_mkt, dummy, relativesmall)]

setnames(fund, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# social
load("social_ret.RData")
load("socialchar.RData")
social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)]

social <- social.char[social.ret, on = .(fund, year, quarter)]
# social id 
socialid <- social[order(fund)
	][, .SD[1], keyby = .(fund)
	][, id := 900001:900044
	][, .(fund, id)]

# match
social <- socialid[social, on = .(fund)
	][, .(id, social_company, date, year, month, quarter, social_style2, social_size, social_age, ret.s, ret.s_mkt)
	][, dummy := 1
	][, realtivesmall := 1]


setnames(social, 2, c("company"))
setnames(social, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# match rbindlist
data <- rbindlist(list(fund, social))

# company
load("companychar.RData")
data <- company.char[data, on = .(company, date)]

data <- data[order(id, year, month)
	][, ret.1 := shift(ret, n = 1, fill = NA, type = "lead"), keyby = .(id)]

write.csv(data, "C://Users//shenfan//Desktop//relativesmallreceiptdeletetop25.csv")



#################################################################################################################################### try
a <- fread("C:/Users/shenfan/Desktop/社保/tables/new/relativebigprovision_1109(1).csv")
b <- fread("C:/Users/shenfan/Desktop/社保/tables/new/relativebigprovision_1114.csv")

b <- b[, log_size := log(size)
	][, log_age := log(age)
	][order(id, year, month)
	][, ret_mkt.1 := shift(ret_mkt, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, log_ret.1 := log(ret.1 + 1)
	][, log_ret := log(ret + 1)]

# match
Date <- as.data.table(seq.Date(from = as.Date("2004/01/01", format = "%Y/%m/%d"), by = "month", length.out = 200))
setnames(Date, 1, "date")
Date <- Date[, year := year(date)
	][, month := month(date)]

b <- Date[b, on = .(year, month)]

# 曾经是
former <- b[, .(id, company, date, ret, dummy)
	][dummy == 1
	][, rank := ntile(ret, 5), keyby = .(company, date)
	][rank == 5
	][order(id, date)
	][, .SD[1], keyby = .(id)
	][, .(id, date)
	][, dummy := 1]

b <- b[, dummy := NULL]

b <- former[b, on = .(id, date)
	][, dummy := shift(dummy, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][, dummy := ifelse(is.na(dummy), 0, 1)
	][, dummy := cumsum(dummy), keyby = .(id)]

write.csv(b, "C://Users//shenfan//Desktop//relativebigprovision1116.csv")

felm(ret.1 ~ relativebig + dummy + relativebig * dummy + ret + log_size + log_age + logcompany_size + logcompany_age | year + style + id, b) %>% summary()




liner1 <- felm(ret.1 ~ relativebig + dummy + relativebig * dummy + ret + log_size + log_age + logcompany_size + logcompany_age | year + style + id, b)

liner2 <- felm(ret.1 ~ relativebig + dummy + relativebig * dummy + ret + log_size + log_age + logcompany_size + logcompany_age | date + style + id, b)

liner3 <- felm(ret.1 ~ relativebig + dummy + relativebig * dummy + ret + log_size + log_age + logcompany_size + logcompany_age | year + style + company, b)

liner4 <- felm(ret.1 ~ relativebig + dummy + relativebig * dummy + ret + log_size + log_age + logcompany_size + logcompany_age | date + style + company, b)

stargazer(liner1, liner2, liner3, liner4, type = "html", out = "C:/Users/shenfan/Desktop/reg.doc", report = ('vc*t'))


######################################################################################################################################### annual
#load("fund_ret.RData")
load("fundper.RData")
load("fundchar.RData")

fundper <- fundper[, .(id, year, month, mkt.12, ret.f.12, alpha_four)
	][month == 12]

fund.char <- fund.char[, year := year(date)
	][, month := month(date)]

fund <- fund.char[fundper, on = .(id, year, month)]

# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event22.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")
event <- event[, socialstart := as.Date(socialstart)]

fund <- event[fund, on = .(company)
	][, dummy := ifelse(as.Date(date) > as.Date(socialstart), 1, 0)
	][, dummy := ifelse(is.na(dummy), 0, dummy)]

# 删除company里面基金少于4个
fund <- fund[, N := (.N), keyby = .(year, month, company)
	][N > 3]

# relative big
fund <- fund[, relativebig := ntile(fund_size, 4), keyby = .(year, month, company)
	][, relativebig := ifelse(relativebig == 4, 1, 0)]

fund <- fund[order(id, year, month)
	][, .(id, company, date, year, month, fund_style, fund_size, fund_age, ret.f.12, mkt.12, alpha_four, dummy, relativebig)]

setnames(fund, 6:10, c("style", "size", "age", "ret.12", "mkt.12"))

# social
#load("social_ret.RData")
load("socialper.RData")
load("socialchar.RData")
socialper <- socialper[, .(fund, year, month, mkt.12, ret.s.12, alpha_four)
	][month == 12]

social.char <- social.char[, year := year(date)
	][, month := month(date)]

social <- social.char[socialper, on = .(fund, year, month)]
# social id 
socialid <- social[order(fund)
	][, .SD[1], keyby = .(fund)
	][, id := 900001:900042
	][, .(fund, id)]

# match
social <- socialid[social, on = .(fund)
	][, .(id, social_company, date, year, month, social_style2, social_size, social_age, ret.s.12, mkt.12, alpha_four)
	][, dummy := 0
	][, relativebig := 0
	][, social_size := social_size * 10000]


setnames(social, 2, c("company"))
setnames(social, 6:10, c("style", "size", "age", "ret.12", "mkt.12"))

# match rbindlist
data <- rbindlist(list(fund, social))

# company
load("companychar.RData")
data <- company.char[data, on = .(company, date)]

data <- data[order(id, year, month)
	][, ret_mkt := ret.12 - mkt.12
	][, ret.12.1 := shift(ret.12, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, ret_mkt.1 := shift(ret_mkt, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, alpha_four.1 := shift(alpha_four, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, log_size := log(size)
	][, log_age := log(age)]

write.csv(data, "C://Users//shenfan//Desktop//relativebigannualprovision.csv")


felm(ret_mkt.1 ~ relativebig + dummy + relativebig * dummy + ret_mkt + log_size + log_age + logcompany_size + logcompany_age | year + style + id, data) %>% summary()



####################################################################################################################################### 新版版 2019-11-17 new relative big or small
# relativesize 是 自身/mean all（自身）25%
# or ntile 2
# topelseinfirm 除去top20%
load("fund_ret.RData")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, fund_category := NULL]

fund <- fund.char[fund.ret, on = .(id, year, quarter)]

# 删除company里面基金少于4个
fund <- fund[, N := (.N), keyby = .(year, month, company)
	][N > 3]

# relative big
fund <- fund[!is.na(fund_size)
	][, allsize := sum(fund_size), keyby = .(company, year, month)
	][, relativesize := fund_size *(N-1) / (allsize - fund_size), keyby = .(company, year, month)
	][, relativebig.g := ntile(relativesize, 4), keyby = .(company, year, month)
	#][, relativebig := ntile(fund_size, 2), keyby = .(year, month, company)
	][, relativebig := ifelse(relativebig.g == 4, 1, 0)]

# topelse
fund <- fund[, rank := ntile(ret.f, 5), keyby = .(company, year, month)
	][, topelse := ifelse(rank == 5, 0, 1)]

fund <- fund[order(id, year, month)
	][, .(id, company, date, year, month, quarter, fund_style, fund_size, fund_age, ret.f, ret.f_mkt, relativebig, topelse)]

setnames(fund, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# social
load("social_ret.RData")
load("socialchar.RData")
social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)]

social <- social.char[social.ret, on = .(fund, year, quarter)]
# social id 
socialid <- social[order(fund)
	][, .SD[1], keyby = .(fund)
	][, id := 900001:900044
	][, .(fund, id)]

# match
social <- socialid[social, on = .(fund), nomatch = 0
	][, social_size := social_size * 10000
	][, .(id, social_company, date, year, month, quarter, social_style2, social_size, social_age, ret.s, ret.s_mkt)
	][, relativebig := 0
	][, topelse := 0]
	#][, social := 1]

setnames(social, 2, c("company"))
setnames(social, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# match rbindlist
data <- rbindlist(list(fund, social))

# benchmark
benchmark <- fund[!is.na(style)
	][, .(ret.bench = mean(ret)), keyby = .(year, month, style)]

# match
data <- benchmark[data, on = .(style, year, month)]

# 删除是不是social的
# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event22.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")
event <- event[, socialstart := as.Date(socialstart)]

data <- event[data, on = .(company), nomatch = 0]
#][, social := ifelse(as.Date(date) > as.Date(socialstart), 1, 0)
#][, dummy := ifelse(is.na(social), 0, social)]

## relativesize
#data <- data[!is.na(size)
	#][, allsize := sum(size), keyby = .(company, year, month)
	#][, relativesize := size / (allsize - size)
	#][, relative.g := ntile(relativesize, 4), keyby = .(company, year, month)
	#][, relativebig := ifelse(relative.g == 4, 1, 0)]

#data <- data[, social := ifelse(id > ("900000"), 1, 0)]

#a <- data[, .N, keyby = .(social, relativebig)]

# company
load("companychar.RData")
data <- company.char[data, on = .(company, date)]

data <- data[order(id, year, month)
	][, ret.1 := shift(ret, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, ret_mkt.1 := shift(ret_mkt, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, ret_bench := ret - ret.bench
	][, ret_bench.1 := shift(ret_bench, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, log_size := log(size)
	][, log_age := log(age)]


write.csv(data, "C://Users//shenfan//Desktop//relativebigprovisionbenchnonsocial.csv")

felm(ret_bench.1 ~ relativebig + topelse + relativebig * topelse  + ret_bench + log_size + log_age + logcompany_size + logcompany_age | year + style + id, data) %>% summary()

#############################################################################
# panel A dummy top dummy social 
load("fund_ret.RData")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, fund_category := NULL]

fund <- fund.char[fund.ret, on = .(id, year, quarter)]

# 删除company里面基金少于4个
fund <- fund[, N := (.N), keyby = .(year, month, company)
	][N > 3]

# relative big
fund <- fund[!is.na(fund_size)
	][, allsize := sum(fund_size), keyby = .(company, year, month)
	][, relativesize := fund_size * (N - 1) / (allsize - fund_size), keyby = .(company, year, month)
	][, relativebig.g := ntile(relativesize, 4), keyby = .(company, year, month)
	#][, relativebig := ntile(fund_size, 2), keyby = .(year, month, company)
	][, relativesmall := ifelse(relativebig.g == 1, 1, 0)]

# topelse
fund <- fund[, rank := ntile(ret.f, 5), keyby = .(company, year, month)
	][, top := ifelse(rank == 5, 1, 0)]

fund <- fund[order(id, year, month)
	][, .(id, company, date, year, month, quarter, fund_style, fund_size, fund_age, ret.f, ret.f_mkt, relativesmall, top)
	][, social := 0]

setnames(fund, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# social
load("social_ret.RData")
load("socialchar.RData")
social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)]

social <- social.char[social.ret, on = .(fund, year, quarter)]
# social id 
socialid <- social[order(fund)
	][, .SD[1], keyby = .(fund)
	][, id := 900001:900044
	][, .(fund, id)]

# match
social <- socialid[social, on = .(fund)
	][, social_size := social_size * 10000
	][, .(id, social_company, date, year, month, quarter, social_style2, social_size, social_age, ret.s, ret.s_mkt)
	][, relativesmall := 1
	][, top := 0
	][, social := 1]

setnames(social, 2, c("company"))
setnames(social, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# match rbindlist
data <- rbindlist(list(fund, social))

# benchmark
benchmark <- fund[!is.na(style)
	][, .(ret.bench = mean(ret)), keyby = .(year, month, style)]

# match
data <- benchmark[data, on = .(style, year, month)]

## 删除是否是社保
# event
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event22.xlsx")
event <- as.data.table(event)
setnames(event, "fund.company", "company")
event <- event[, socialstart := as.Date(socialstart)]

data <- event[data, on = .(company), nomatch = 0]
#][, social := ifelse(as.Date(date) > as.Date(socialstart), 1, 0)
#][, dummy := ifelse(is.na(social), 0, social)]

## relativesize
#data <- data[!is.na(size)
#][, allsize := sum(size), keyby = .(company, year, month)
#][, relativesize := size / (allsize - size)
#][, relative.g := ntile(relativesize, 4), keyby = .(company, year, month)
#][, relativebig := ifelse(relative.g == 4, 1, 0)]

#data <- data[, social := ifelse(id > ("900000"), 1, 0)]

#a <- data[, .N, keyby = .(social, relativebig)]

# company
load("companychar.RData")
data <- company.char[data, on = .(company, date)]

data <- data[order(id, year, month)
	][, ret.1 := shift(ret, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, ret_mkt.1 := shift(ret_mkt, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, ret_bench := ret - ret.bench
	][, ret_bench.1 := shift(ret_bench, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, log_size := log(size)
	][, log_age := log(age)]

felm(ret_bench.1 ~ top + social + ret_bench + log_size + log_age + logcompany_size + logcompany_age | year + style + company, data) %>% summary()

write.csv(data, "C://Users//shenfan//Desktop//relativesmallreceiptbenchnonsocial.csv")