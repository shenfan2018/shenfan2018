# 先按照overlap做
# 这里用的social return是两种
# 这里用的char是new里面的，quarterly的

# add control variables
# 构建
load("socialpf.RData")
setnames(social, 1, "social.name")
social.name <- social[order(social.name)
	][, .(social.name = unique(social.name))]
social.name <- social.name[["social.name"]]
# 所有基金名称
load("portfolio.RData")
mutual <- portfolio[order(id)
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

# 社保monthly return 2
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/social_ret.RData")
setnames(social.ret, "ret.s", "ret.s2")
data <- social.ret[data, on = .(fund, year, month), nomatch = 0]

#基金的monthly return
load('fund_ret.RData')
data <- fund.ret[data, on = .(id, year, sem, month), nomatch = 0]

# control variables char
# fund
load('C:/Users/shenfan/source/repos/shenfan2018/social security fund new/fundchar.RData')
fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(id, company, fund_style, year, quarter, fund_age, fund_size, logfund_age, logfund_size)]

data <- fund.char[data, on = .(id, year, quarter)]

# social
load('C:/Users/shenfan/source/repos/shenfan2018/social security fund new/socialchar.RData')
social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(fund, social_company, social_style, social_style2, year, quarter, social_size, social_age, logsocial_size, logsocial_age)]

data <- social.char[data, on = .(fund, year, quarter)]

# company
load('C:/Users/shenfan/source/repos/shenfan2018/social security fund new/companychar.RData')
company.char <- company.char[, year := year(date)
	][, quarter := quarter(date)
	][, .(company, year, quarter, company_size, company_size_all, company_age)
	][, logcompany_size := log(company_size)
	][, logcompany_size_all := log(company_size_all)
	][, logcompany_age := log(company_age)]

data <- company.char[data, on = .(company, year, quarter)]

# Dummy_same_family
data <- data[, same_family := ifelse(social_company == company, 1, 0)
	][, same_family := ifelse(is.na(same_family), 0, same_family)]

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
	roll[[i]] <- matchit(same_family ~ overlap2, a[tag == i], method = "nearest", ratio = 4) %>% match.data() %>% as.data.table()
}
syf <- rbindlist(roll, fill = T)

syf.PSM <- syf[, .(fund, id, year, month)]

data.PSM <- syf.PSM[data, on = .(fund, id, year, month), nomatch = 0]

######### 目前为止
# add oppositetrade
load("oppositetrade.RData")
opposite.trade <- opposite.trade[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(fund, id, year, sem, opposite_num, opposite_sum, opposite_min, opposite_dummy)]

# match
data.PSM <- opposite.trade[data.PSM, on = .(fund, id, year, sem)]

## add synthetictrade
load("synthetictrade.RData")
synthetic.trade <- synthetic.trade[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(fund, id, year, sem, synthetic_num, synthetic_sum, synthetic_min, synthetic_dummy)]

# match
data.PSM <- synthetic.trade[data.PSM, on = .(fund, id, year, sem)]

data.PSM[is.na(data.PSM)] <- 0

data.PSM <- data.PSM[, dif := ret.s - ret.f
	][, dif.2 := ret.s - ret.f.raw
	][, dif.3 := ret.s2 - ret.f
	][, dif.4 := ret.s2 - ret.f.raw]

write.csv(data.PSM, "C://Users//shenfan//Desktop//socialPSM-ratio4.csv")

# opposite_num, opposite_sum, opposite_dummy, opposite_min
# synthetic_num, synthetic_sum, synthetic_dummy, synthetic_min

# lfe
# felm(dif ~ same_family * opposite_num + logfund_size + logfund_age + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + company + fund_style, data.PSM) %>% summary()

# felm(dif ~ same_family + social_age + social_size + company_size + company_age + fund_size + fund_age | year + company + fund_style, data.PSM) %>% summary()

# plm(dif ~ same_family + social_age + social_size + company_size + company_age + fund_size + fund_age, data.PSM, effect = "twoways", model = "within", index = c("year", "company")) %>% summary()
