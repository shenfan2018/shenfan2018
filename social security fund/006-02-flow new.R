# fund characterstic + performance
load("fundper.RData")
load("fund_ret.RData")
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/fundchar.RData")

# match
data <- fund.char
data <- data[, year := year(date)
	][, month := month(date)]

#data <- data[, lapply("log", str_c, colnames(data[, 7:10])) %>% unlist() := lapply(.SD[, 7:10], log)]

## match
fundper <- fundper[, c("ret.f.raw", "sem") := NULL]

data <- fundper[data, on = .(id, year, month)]

# add company char
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/companychar.RData")
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

write.csv(data, "C://Users//shenfan//Desktop//netflowfund.csv")


##################################################################
### social security fund
load("socialper.RData")
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/socialchar.RData")

# match
data <- social.char
data <- data[, year := year(date)
	][, month := month(date)
	][, .(fund, social_company, social_style, social_style2, date, year, month, social_flow, logsocial_size, logsocial_age)]

#data <- data[, lapply("log", str_c, colnames(data[, 6:7])) %>% unlist() := lapply(.SD[, 6:7], log)]

# match
socialper <- socialper[, c("ret.s", "sem") := NULL]

data <- socialper[data, on = .(fund, year, month)]

## wins flow wins过了
#library(DescTools)
#data <- data[, colnames(data[, 10:12]) := lapply(.SD[, 10:12], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# add company char
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/companychar.RData")
company.char <- company.char[, .(company, date, logcompany_age, logcompany_size, company_flow)]
setnames(company.char, "company", "social_company")

# match
data <- company.char[data, on = .(social_company, date)]

# 滞后
data <- data[, social_flow.1 := shift(social_flow, n = 1, fill = NA, type = "lead"), keyby = .(fund)]

write.csv(data, "C://Users//shenfan//Desktop//netflowsocial.csv")
