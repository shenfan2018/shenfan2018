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

#基金的monthly return
load('fund_ret.RData')
data <- fund.ret[data, on = .(id, year, sem, month), nomatch = 0]

# control variables char
# fund
load('fundchar.RData')
fund.char <- fund.char[, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 6, 1, 2)
	][, .(id, year, sem, fund_age, fund_size, company, company_age, company_size)]

data <- fund.char[data, on = .(id, year, sem)]

# social
load('socialchar.RData')
social.char <- social.char[, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 6, 1, 2)
	][, .(fund, year, sem, social_age, social_size, socialcompany)]

data <- social.char[data, on = .(fund, year, sem)]

# Dummy_same_family
data <- data[, same_family := ifelse(socialcompany == company, 1, 0)
	][, same_family := ifelse(is.na(same_family), 0, same_family)]


################################################################# PSM
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


#write.csv(data.PSM, "C://Users//shenfan//Desktop//socialPSMoverlap1-ratio4.csv")


# dif
data.PSM <- data.PSM[, dif := ret.s - ret.f
	][, dif.2 := ret.s - ret.f.raw]

data.PSM[, felm(dif ~ same_family + fund_age + fund_size + social_age + social_size + company_age + company_size | fund + year)] %>% summary()

lm(dif ~ same_family + factor(company) + factor(date), data.PSM) %>% summary()


# overlap2, ratio = 1 , dif, company + date p 0.0784. t = 1.76, Estimate 0.0013204
# overlap2, ratio = 2, dif.2 or dif, company + date, p 0.0323 *, t = 2.141 Estimate 0.001267




plm(dif.2 ~ same_family, data.PSM, model = "within", effect = "twoways", index = c("company", "date")) %>% summary()

data[, felm(dif ~ same_family + fund_age + fund_size + social_age + social_size | company + year)] %>% summary()


