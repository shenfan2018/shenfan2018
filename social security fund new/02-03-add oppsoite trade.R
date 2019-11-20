# family help
# add control variables
# 构建
load("socialpf.RData")
setnames(social, 1, "social.name")
social.name <- social[order(social.name)
	][, .(social.name = unique(social.name))]
social.name <- social.name[["social.name"]]
# 所有基金名称
load("fundpf.RData")
mutual <- fundpf[order(id)
	][, .(id = unique(id))]
mutual <- mutual[["id"]]

date <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间.xlsx")
date <- date[["date"]]

#perfect 三列相拼
data <- CJ(fund = social.name, id = mutual, date = date)
data <- data[, year := year(date)
	][, month := month(date)
	][year < 2018]

# 社保的monthly return
load("social_ret.RData")
data <- social.ret[data, on = .(fund, year, month), nomatch = 0]

#基金的monthly return
load('fund_ret.RData')
data <- fund.ret[data, on = .(id, year, month, quarter), nomatch = 0]

# control variables char
# fund
load('fundchar.RData')
fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, fund_style, year, quarter, fund_flow, logfund_size, logfund_age)]

data <- fund.char[data, on = .(id, year, quarter)]

# social
load('socialchar.RData')
social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(fund, social_company, social_style, social_style2, year, quarter, social_flow, logsocial_size, logsocial_age)]

data <- social.char[data, on = .(fund, year, quarter)]

# company
load("companychar.RData")
company.char <- company.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(company, company.q, year, quarter, company_flow, logcompany_size, logcompany_size_all, logcompany_age)]

data <- company.char[data, on = .(company, year, quarter)]

# Dummy_same_family
# 新版，剔除same family为NA的
data <- data[, same_family := ifelse(social_company == company, 1, 0)
	][!is.na(same_family)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]
#	][, same_family := ifelse(is.na(same_family), 0, same_family)]

################################################################### 第一种 依旧按照overlap
############################## PSM
# add overlap
load('overlap.RData')
# overlap滞后
overlap <- overlap[, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 2, 1)
	][, year := year(date)
	][, year := ifelse(quarter == 2, year, year + 1)
	][, c("date", "quarter") := NULL]

# match
data <- overlap[data, on = .(id, fund, year, sem)]

# 根据overlap PSM
library(MatchIt)
a <- data[, .(id, fund, year, month, same_family, overlap1, overlap2)
	][!overlap1 == 0
	][order(year, month, fund, - same_family)]

unique <- a[, .SD[1], keyby = .(fund, year, month)
	][same_family == 1
	][, tag := 1:(.N)
	][, .(fund, year, month, tag)]

a <- unique[a, on = .(fund, year, month), nomatch = 0]

roll <- list()
for (i in 1:max(a[, tag])) {
	roll[[i]] <- matchit(same_family ~ overlap2, a[tag == i], method = "nearest", ratio = 1) %>% match.data() %>% as.data.table()
}
syf <- rbindlist(roll, fill = T)

syf.PSM <- syf[, .(fund, id, year, month)]

data.PSM <- syf.PSM[data, on = .(fund, id, year, month), nomatch = 0]

######### 目前为止
# add oppositetrade
load("oppositetrade.RData")
# opposite 滞后
opposite.trade <- opposite.trade[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 2, 1)
	][, year := ifelse(quarter == 2, year, year + 1)
	][, .(fund, id, year, sem, opposite_num, opposite_sum, opposite_min, opposite_dummy)]

# match
data.PSM <- opposite.trade[data.PSM, on = .(fund, id, year, sem)]

## add synthetictrade
load("synthetictrade.RData")
synthetic.trade <- synthetic.trade[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 2, 1)
	][, year := ifelse(quarter == 2, year, year + 1)
	][, .(fund, id, year, sem, synthetic_num, synthetic_sum, synthetic_min, synthetic_dummy)]

# match
data.PSM <- synthetic.trade[data.PSM, on = .(fund, id, year, sem)]

data.PSM[is.na(data.PSM)] <- 0

data.PSM <- data.PSM[, dif := ret.s - ret.f]

write.csv(data.PSM, "C://Users//shenfan//Desktop//overlapoppositetrade.csv")


####################################### 第二种 没有overlap的事情，删除没有opposite为0的
# !opposite 也是下一期的
load("oppositetrade.RData")
opposite.trade <- opposite.trade[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 2, 1)
	][, year := ifelse(quarter == 2, year, year + 1)
	][, .(fund, id, year, sem, opposite_num, opposite_sum, opposite_min, opposite_dummy)]

# match 删除没有opposite
data <- opposite.trade[data, on = .(fund, id, year, sem), nomatch = 0]

write.csv(data, "C://Users//shenfan//Desktop//!opoppositetrade.csv")


###################################################################第三种，按照opposite trade PSM
############################## PSM
# add overlap
load('oppositetrade.RData')
# overlap滞后
opposite.trade <- opposite.trade[, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 2, 1)
	][, year := year(date)
	][, year := ifelse(quarter == 2, year, year + 1)
	][, c("date", "quarter") := NULL]

# match
data <- opposite.trade[data, on = .(id, fund, year, sem)]

# 根据opposite PSM
library(MatchIt)
a <- data[, .(id, fund, year, month, same_family, opposite_sum, opposite_min)
	][!opposite_min == 0
	][order(year, month, fund, - same_family)]

unique <- a[, .SD[1], keyby = .(fund, year, month)
	][same_family == 1
	][, tag := 1:(.N)
	][, .(fund, year, month, tag)]

a <- unique[a, on = .(fund, year, month), nomatch = 0]

roll <- list()
for (i in 1:max(a[, tag])) {
	roll[[i]] <- matchit(same_family ~ opposite_sum + opposite_min, a[tag == i], method = "nearest", ratio = 1) %>% match.data() %>% as.data.table()
}
syf <- rbindlist(roll, fill = T)

syf.PSM <- syf[, .(fund, id, year, month)]

data.PSM <- syf.PSM[data, on = .(fund, id, year, month), nomatch = 0]

write.csv(data.PSM, "C://Users//shenfan//Desktop//oppositetradePSM.csv")
