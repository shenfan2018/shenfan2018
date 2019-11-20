# 搞呀
# 所有社保名称
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 3, "social.name")
social <- social[order(social.name)
	][, .(social.name)
	][, .(social.name = unique(social.name))]
social <- social[["social.name"]]

######################################################### 新一轮
load("socialpf.RData")
setnames(social, 1, "social.name")
social.name <- social[order(social.name)
	][, .(social.name)
	][, .(social.name = unique(social.name))]
social.name <- social.name[["social.name"]]
# 所有基金名称
load("portfolio.RData")
mutual <- portfolio[order(id)
	][, .(id)
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
load("fund-NAV.RData")
fund <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(return.f = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)]

data <- fund[data, on = .(id, year, month), nomatch = 0]


# 匹配fund family
# social
social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "fund.company.s"))
social.m <- social.m[, fund.company.s := str_replace_all(fund.company.s, "管理有限公司", "")
	][!is.na(fund.company.s), .SD]

data <- social.m[data, on = .(fund)]

# fund
fund.co <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人.xlsx")
fund.co <- as.data.table(fund.co)
setnames(fund.co, 1:3, c("code", "name", "fund.company"))
fund.co <- fund.co[, id := substring(code, 1, 6)
	][, c("id", "fund.company")]

data <- fund.co[data, on = .(id)]

data <- data[, same := ifelse(fund.company == fund.company.s, 1, 0)]

data <- data[, dif := ret - return.f
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

#  加入overlap(这里要运行06-overlap)
data <- overlap[data, on = .(id, year, sem), nomatch = 0]

a <- data[!is.na(dif)
	][!is.na(same)]


#b <- a[same == 0
#	][-(((.N) * 0.2):(.N))]
#c <- a[same == 1]
#d <- rbindlist(list(b, c))

plm(dif ~ same, a, model = "within", effect = "twoways", index = c("fund.company.s", "date")) %>% summary()

a[, felm(dif ~ same | fund.company.s + year)] %>% summary()

a[, felm(dif ~ same + overlap1 + same * overlap1 | fund.company.s + year)] %>% summary()

a[, felm(dif ~ same + overlap2 + same * overlap2 | fund.company.s + year)] %>% summary()

a[, felm(dif ~ overlap2 | fund.company.s + year)] %>% summary()


################################################################# 改
################################################### 基金与社保的相似度
load("socialpf.RData")
social <- social[order(fund, year, sem)
	][, .(fund, fund.company, date, year, sem, stock.id, MV.current)]
social.name <- social[order(fund)
	][, .(social.name = unique(fund))]
social.name <- social.name[["social.name"]]

social.1 <- social[, buy := 1
	][, .(fund, date, stock.id, buy, MV.current)]

load("portfolio.RData")

data <- list()
for (i in 1:26) {
	social.name1 <- social.name[i]

	data[[i]] <- portfolio[, fund := social.name1]
	data[[i]] <- social.1[data[[i]], on = .(fund, date, stock.id)
	][is.na(buy), buy := 0
	][, .(overlap1 = sum(buy) / (.N), overlap2 = sum(proportion.stock * buy) / sum(proportion.stock)), keyby = .(id, date)
	][, fund := social.name1]
	rbindlist(data)
}

roll <- data.table(i = 1:26, data)
roll <- roll[1:26]
syf <- roll[, rbindlist(.SD[['data']]), by = i]

syf <- syf[order(date)
	][, group := ntile(overlap2, 10), keyby = .(date)
	][date < as.Date("2017-12-31")]

high.similar <- syf[group == 10
	][, .(id, date, fund)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

social.name <- high.similar[, .(fund)
	][, .(fund = unique(fund))]
social.name <- social.name[["fund"]]
fund.name <- high.similar[, .(id)
	][, .(id = unique(id))]
id <- fund.name[["id"]]
date <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间.xlsx")
date <- date[["date"]]
#perfect 三列相拼
SJ <- CJ(fund = social.name, id = id, date = date)
SJ <- SJ[, year := year(date)
	][, month := month(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][year < 2018]
# 删除不需要的
SJ <- high.similar[SJ, on = .(fund, id, year, sem), nomatch = 0
	][, .(fund, id, year, month, i.date)]

# 社保的monthly return
load("social_ret.RData")
SJ<- social.ret[SJ, on = .(fund, year, month), nomatch = 0]

#基金的monthly return
load("fund-NAV.RData")
fund <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(return.f = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)]

SJ <- fund[SJ, on = .(id, year, month), nomatch = 0]


# 匹配fund family
# social
social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "fund.company.s"))
social.m <- social.m[, fund.company.s := str_replace_all(fund.company.s, "管理有限公司", "")
	][!is.na(fund.company.s), .SD]

SJ <- social.m[SJ, on = .(fund)]

# fund
fund.co <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人.xlsx")
fund.co <- as.data.table(fund.co)
setnames(fund.co, 1:3, c("code", "name", "fund.company"))
fund.co <- fund.co[, id := substring(code, 1, 6)
	][, c("id", "fund.company")]

SJ <- fund.co[SJ, on = .(id)]

SJ <- SJ[, same := ifelse(fund.company == fund.company.s, 1, 0)
	][, dif := ret - return.f]


SJ[, felm(dif ~ same | fund.company.s + year)] %>% summary()

a <- SJ[, felm(dif ~ same | fund.company.s + year)]


stargazer(a, type = "html", out = "C:/Users/shenfan/Desktop/社保/tables/new/family.doc", add.lines = list(c("Fund company", "yes"), c("year", "yes")))













