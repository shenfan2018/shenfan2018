# 社保 size
load("socialpf.RData")
social <- social[, .(social_size = sum(MV.current, na.rm = TRUE)), keyby = .(fund, date)
	][, logsocial_size := log(social_size)]

social_age <- social[, .SD[1], keyby = .(fund)
	][, social_date := date
	][, .(fund, social_date)]

social.char <- social_age[social, on = .(fund)
	][, days := difftime(date, social_date, units = c("days"))
	][, social_age := as.numeric(days) / 365 + 0.5
	][, logsocial_age := log(social_age)
	][, .(fund, date, social_size, social_age, logsocial_size, logsocial_age)]

# add social company
# 匹配fund family
social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人3.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "social_company"))
social.m <- social.m[, social_company := str_replace_all(social_company, "管理有限公司", "")
	][!is.na(social_company), .SD]
social.char <- social.m[social.char, on = .(fund)]

# add flow * (1 + ret)
# 改
load("social_ret.RData")
social.ret <- social.ret[, .(fund, year, month, ret.s)
	][, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(ret.s = prod(1 + ret.s) - 1), keyby = .(fund, year, quarter)]

# match date
date.q <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date.q <- as.data.table(date.q)
date.q <- date.q[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, .SD[.N], keyby = .(year, quarter)]
# match
social.ret <- date.q[social.ret, on = .(year, quarter)
	][, .(fund, date, ret.s)]

flow <- social.char[social.ret, on = .(fund, date)
	][, social_size.1 := shift(social_size, n = 1, fill = NA, type = "lag"), keyby = .(fund)
	][, social_size_exp := (1 + ret.s) * social_size.1
	][, social_flow := (social_size - social_size_exp) / social_size_exp
	][, .(fund, date, social_flow)]

# match
social.char <- flow[social.char, on = .(fund, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# 根据overlap看style
load("overlap.RData")
overlap <- overlap[!overlap1 == 0
	][order(fund, date, - overlap2, - overlap1)]
load("fundchar.RData")
style <- fund.char[, .(id, date, fund_style)]
overlap <- style[overlap, on = .(id, date)
	][, .SD[1], keyby = .(fund, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(fund, year, sem, fund_style)]
setnames(overlap, "fund_style", "social_style")
# match
social.char <- overlap[social.char, on = .(fund, year, sem)]

# 第二种style
style2 <- overlap[, .(N = .N), keyby = .(fund, social_style)
	][order(fund, - N)
	][, .SD[1], keyby = .(fund)
	][, .(fund, social_style)]
setnames(style2, "social_style", "social_style2")

social.char <- style2[social.char, on = .(fund)
	][, .(fund, social_company, social_style, social_style2, date, social_flow, social_size, social_age, logsocial_size, logsocial_age)]

# social_flow Winsorize
library(DescTools)
social.char <- social.char[, colnames(social.char[, 6]) := lapply(.SD[, 6], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

save(social.char, file = 'socialchar.RData')

########################################## mutual fund char
# 改fund 2004-2018
time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31","2018-03-31", "2018-06-30", "2018-09-30", "2018-12-31")
#添加市值
marketvalue <- read_excel("C://Users//shenfan//Desktop//社保//data//基金资产净值3.xlsx")
marketvalue <- as.data.table(marketvalue)
setnames(marketvalue, 1:62, c("code", "name", time))
marketvalue <- marketvalue[!is.na(name)]
marketvalue = melt(marketvalue, id.vars = c("code", "name"), measure.vars = time)
setnames(marketvalue, c("value", "variable"), c("fund_size", "date"))
marketvalue <- marketvalue[, date := as.Date(as.character(date))
	][, logfund_size := as.data.table(log(fund_size))
	][order(code, date)]

# 加入
data <- marketvalue

# flow
fundflow <- read_excel("C:/Users/shenfan/Desktop/社保/data/单季度净申购赎回率3.xlsx")
fundflow <- as.data.table(fundflow)
setnames(fundflow, 1:62, c("code", "name", time))
fundflow <- fundflow[!is.na(name)]
fundflow <- melt(fundflow, id.vars = c("code", "name"), measure.vars = time)
setnames(fundflow, "variable", "date")
setnames(fundflow, 4, c("netflow"))
fundflow <- fundflow[, date := as.Date(date)]

data <- fundflow[data, on = .(code, name, date)]

# 添加fund_age
fundage <- read_excel("C:/Users/shenfan/Desktop/社保/data/成立日期3.xlsx")
fundage <- as.data.table(fundage)
setnames(fundage, 1:3, c("code", "name", "fund_start"))
data <- data[fundage, on = .(code, name), nomatch = 0
	][as.Date(date) > as.Date(fund_start)
	][, fund_age := as.numeric(difftime(date, fund_start, units = c("days")) / 365)
	][, logfund_age := log(fund_age)]

# add new style
time <- c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31", "2018-06-30", "2018-12-31")

style <- read_excel("C:/Users/shenfan/Desktop/社保/data/fundstyle3.xlsx")
style <- as.data.table(style)
setnames(style, 1:32, c("code", "name", time))
style <- style[!is.na(name)]
style = melt(style, id.vars = c("code", "name"), measure.vars = time)
setnames(style, c("value", "variable"), c("fund_style", "date"))

style <- style[, date := as.Date(as.character(date))
	][order(code, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(code, name, year, sem, fund_style)]

# match fund company
# match 基金公司 简称
fundcompany <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人3.xlsx")
fundcompany <- as.data.table(fundcompany)
setnames(fundcompany, 1:4, c("code", "name", "company.q", "company"))
fundcompany <- fundcompany[!is.na(name)
	][, .(code, company)]

data <- fundcompany[data, on = .(code)]

# match fund character
data <- data[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, id := substring(code, 1, 6)]
data <- style[data, on = .(code, year, sem)
	][, .(code, id, name, company, date, fund_style, fund_start, netflow, fund_age, fund_size, logfund_age, logfund_size)]

setnames(data, "netflow", "fund_flow")

# delete1 style 
# style 这个存在一些问题，有些基金变更了类型
#category <- read_excel("C:/Users/shenfan/Desktop/社保/data/windstyle3.xlsx")
#category <- as.data.table(category)
#setnames(category, 1:3, c("code", "name", "fund.category"))
#category <- category[fund.category == "普通股票型基金" | fund.category == "偏股混合型基金", .SD
	#][, .(code, name, fund.category)]

## match
#data <- category[data, on = .(code, name), nomatch = 0
	#][, id := substring(code, 1, 6)]

# delete 2 fundpf里面的
load("fundpf.RData")
fundpf <- fundpf[order(id)
	][, .(id, fund_category)
	][, .SD[1], keyby = .(id)]

data <- fundpf[data, on = .(id), nomatch = 0]

# fund_flow Winsorize
data <- data[, colnames(data[, 9]) := lapply(.SD[, 9], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

fund.char <- data

save(fund.char, file = 'fundchar.RData')
# 22295——22783

##################################################################
# company char
# company size 用上面的marketvalue
marketvalue <- marketvalue[, .(code, date, fund_size)]

# match 基金公司 简称
fundcompany <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人3.xlsx")
fundcompany <- as.data.table(fundcompany)
setnames(fundcompany, 1:4, c("code", "name", "company.q", "company"))
fundcompany <- fundcompany[!is.na(name)]

# match
data <- fundcompany[marketvalue, on = .(code)
	][!is.na(fund_size)
	][, .(company_size_all = sum(fund_size)), keyby = .(company, company.q, date)]

# age
# 基金公司信息
companyinfo <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金公司规模.xlsx")
companyinfo <- as.data.table(companyinfo)
setnames(companyinfo, 1, c("company.q"))
setnames(companyinfo, 17, "startdate")
companyinfo <- companyinfo[, .(company.q, startdate)
	][, startdate := as.Date(startdate)
	][!is.na(startdate)]

# match
data <- companyinfo[data, on = .(company.q)
	][, company_age := as.numeric((difftime(date, startdate, units = c("days"))) / 365)]

# company flow
load("fund_ret.RData")
fund.ret <- fund.ret[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(ret.f = prod(1 + ret.f.raw) - 1), keyby = .(id, year, quarter)]
# match date
date.q <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date.q <- as.data.table(date.q)
date.q <- date.q[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, .SD[.N], keyby = .(year, quarter)]
# match
fund.ret <- date.q[fund.ret, on = .(year, quarter)]
# match
fundcompany <- fundcompany[, id := substring(code, 1, 6)]
fund.ret <- fundcompany[fund.ret, on = .(id)]

# flow
flow <- fund.ret[marketvalue, on = .(code, date)
	][, fund_size.1 := shift(fund_size, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][, fund_size.ca := fund_size.1 * (1 + ret.f)
	][!is.na(fund_size.ca)
	][, .(company_flow = (sum(fund_size) - sum(fund_size.ca)) / sum(fund_size.1), company_size = sum(fund_size)), keyby = .(company, date)]

# final match
data <- flow[data, on = .(company, date)]

company.char <- data[, .(company.q, company, date, company_size, company_size_all, company_age, company_flow)]

# log
company.char <- company.char[, logcompany_size := log(company_size)
	][, logcompany_size_all := log(company_size_all)
	][, logcompany_age := log(company_age)]

# company_flow Winsorize
company.char <- company.char[, colnames(company.char[, 7]) := lapply(.SD[, 7], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

save(company.char, file = "companychar.RData")



