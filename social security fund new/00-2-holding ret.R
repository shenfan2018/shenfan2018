### 新版本 single social holding return quarterly
load("socialpf.RData")
social <- social[, proportion := MV.current / sum(MV.current, na.rm = TRUE), keyby = .(fund, year, quarter)]

# stock, 这里滞后了，若要同期，则删除最后两行
load("C:/Users/shenfan/source/repos/shenfan2018/Buffett alpha/stock20030101-20190331.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, .(month_ret = prod(Dretwd + 1, na.rm = TRUE) - 1), keyby = .(stock.id, year, quarter, month)]

# 往后滞后了一期，social的
social <- social[, .(fund, stock.id, date, year, quarter, proportion)
	][, year := ifelse(quarter == 4, year + 1, year)
	][, quarter := ifelse(quarter == 4, 1, quarter + 1)]

# 目前ret是 期的持仓算t期的收益
social.ret <- stock.m[social, on = .(stock.id, year, quarter)
	][, .(ret.s = sum(proportion * month_ret, na.rm = TRUE)), keyby = .(fund, year, quarter, month)
	][year < 2019
	][!is.na(month)]

# add mkt
mkt <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
mkt <- mkt[, mkt := mkt_rf + rf
	][, .(year, month, mkt)]

social.ret <- mkt[social.ret, on = .(year, month)
	][, ret.s_mkt := ret.s - mkt
	][, .(fund, year, month, quarter, ret.s, ret.s_mkt)]

save(social.ret, file = "social_ret.RData")


######################################################
# mutual fund raw return
## fund NAV
# NAV daily(NAVA为原始，NAV处理) 2004.1.1-2019.1.1
NAV0 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM0406.xlsx")
NAV00 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM0608.xlsx")
NAV000 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM0810.xlsx")
NAV1 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM1.xlsx")
NAV2 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM2.xlsx")
NAV3 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM3.xlsx")
NAV4 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM4.xlsx")
NAV5 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM5.xlsx")
NAV6 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM6.xlsx")
NAV7 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM7.xlsx")
NAV8 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM8.xlsx")
NAV9 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//业绩 净值//净值表现复权//Fund_NAV_PFM9.xlsx")
NAV <- rbindlist(list(NAV0, NAV00, NAV000, NAV1, NAV2, NAV3, NAV4, NAV5, NAV6, NAV7, NAV8, NAV9))
rm(NAV0, NAV00, NAV000, NAV1, NAV2, NAV3, NAV4, NAV5, NAV6, NAV7, NAV8, NAV9)
#NAV原始数据
data.NAV <- NAV
data.NAV <- as.data.table(data.NAV)
setnames(data.NAV, 1:2, c("date", "id"))
data.NAV <- data.NAV[date != "没有单位'", .SD
	][date != "统计日期'", .SD
	][, AdjustedNAV := as.numeric(as.character(AdjustedNAV))
	][, AdjustedNAVGrowth := as.numeric(as.character(AdjustedNAVGrowth))
	][, date := as.Date(as.character(date))
	][, c("FullName", "NAV") := NULL]

# 根据adjustedNAV自己算return,这里的收益率都是%，都*100
# 排序
data.NAV <- data.NAV[order(id, date)
	][, AdjustedNAVGrowth := AdjustedNAVGrowth / 100]

data.NAV <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(ret.f.raw = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

save(data.NAV, file = "fund_ret_all.RData")


######### category 加入 C H
#load("fund_ret_all.RData")
## category
#category <- read_excel("C:/Users/shenfan/Desktop/社保/data/windstyle3.xlsx")
#category <- as.data.table(category)
#setnames(category, 1:3, c("code", "name", "fund.category"))
#category <- category[, id := substring(code, 1, 6)
	#][fund.category == "普通股票型基金" | fund.category == "偏股混合型基金", .SD
	#][, c("code", "id", "fund.category", "name")]
#fundage <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金时间.xlsx")
#fundage <- as.data.table(fundage)
#setnames(fundage, 1:4, c("code", "name", "fund_start", "fund_end"))
#fundage <- fundage[, fund_start := as.Date(fund_start)
	#][, fund_end := as.Date(fund_end)]
#category <- fundage[category, on = .(code, name)
	#][fund_start < as.Date("2019-01-01")]

#load("fundpf.RData")
#fundpf <- fundpf[order(id)
	#][, .(id, fund_category)
	#][, .SD[1], keyby = .(id)]

#final.category <- fundpf[category, on = .(id)]

## match
# fund.ret <- category[data.NAV, on = .(id), nomatch = 0]


# delete 2 fundpf里面的
load("fundpf.RData")
fundpf <- fundpf[order(id)
	][, .(id, fund_category)
	][, .SD[1], keyby = .(id)]

fund.ret <- fundpf[data.NAV, on = .(id), nomatch = 0]

# add mkt
mkt <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
mkt <- mkt[, mkt := mkt_rf + rf
	][, .(year, month, mkt)]

fund.ret <- mkt[fund.ret, on = .(year, month)
	][, ret.f_mkt := ret.f.raw - mkt
	][, ret.f := ret.f.raw
	][, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(id, fund_category, year, month, quarter, ret.f, ret.f_mkt)]

save(fund.ret, file = "fund_ret.RData")

# 68372 —— 66684
# 原本里面1025只基金，现在就901
# 原本的里面包括了9开头的，各种H
#load("fund_ret.RData")
#a <- fund.ret[order(id)
	#][, .(id=unique(id))]
#fundpf <- fundpf[, yes := 0]
#a <- fundpf[a, on = .(id)]


################################################################
# company performance
# monthly performance

# all sample or sub sample

# sub sample
load("fund_ret.RData")
data.NAV <- fund.ret[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))]

# match size
load("fundchar.RData")
fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, year, quarter, company, fund_category, fund_size)]

data.sub <- fund.char[fund.ret, on = .(id, year, quarter), nomatch = 0
	][!is.na(fund_size)
	][, proportion := fund_size / sum(fund_size), keyby = .(company, year, month)
	][, .(ret.c.raw.sub = sum(proportion * ret.f.raw)), keyby = .(company, year, month)]


# all sample (data.NAV全的，需要size)
# 使用00-3-characterstic marketvalue
marketvalue <- marketvalue[, .(code, date, fund_size)
	][, id := substring(code, 1, 6)]

# match company
fundcompany <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人3.xlsx")
fundcompany <- as.data.table(fundcompany)
setnames(fundcompany, 1:4, c("code", "name", "company.q", "company"))
fundcompany <- fundcompany[!is.na(name)
	][, id := substring(code, 1, 6)
	][, .(id, company)]

marketvalue <- fundcompany[marketvalue, on = .(id), allow.cartesian = TRUE
	][, year := year(date)
	][, quarter := quarter(date)]

load("fund_ret_all.RData")
data.NAV <- data.NAV[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))]
data.all <- marketvalue[data.NAV, on = .(id, year, quarter), nomatch = 0
	][!is.na(fund_size)
	][, proportion := fund_size / sum(fund_size), keyby = .(company, year, month)
	][, .(ret.c.raw.all = sum(proportion * ret.f.raw)), keyby = .(company, year, month)]

# all
company.ret <- data.sub[data.all, on = .(company, year, month)]

setnames(company.ret, c("ret.c.raw.sub", "ret.c.raw.all"), c("ret.c.sub", "ret.c.all"))

# add mkt
mkt <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
mkt <- mkt[, mkt := mkt_rf + rf
	][, .(year, month, mkt)]

company.ret <- mkt[company.ret, on = .(year, month)
	][, ret.c.sub_mkt := ret.c.sub - mkt
	][, ret.c.all_mkt := ret.c.all - mkt
	][, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(company, year, month, quarter, ret.c.sub, ret.c.all, ret.c.sub_mkt, ret.c.all_mkt)]

save(company.ret, file = "company_ret.RData")