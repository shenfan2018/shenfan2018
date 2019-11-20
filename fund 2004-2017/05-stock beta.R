# 股价整理 为2010-2017的日度数据(2003.1.1-2017.12.31)
#stock daily
stock0 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr0.xlsx")
stock1 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr1.xlsx")
stock2 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr2.xlsx")
stock3 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr3.xlsx")
stock4 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr4.xlsx")
stock5 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr5.xlsx")
stock6 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr6.xlsx")
stock7 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr7.xlsx")
stock8 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr8.xlsx")
stock9 <- read_excel("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//个股日度回报率//TRD_Dalyr9.xlsx")
stock.raw <- rbindlist(list(stock0, stock1, stock2, stock3, stock4, stock5, stock6, stock7, stock8, stock9))#stock原文件 勿删
rm(stock0, stock1, stock2, stock3, stock4, stock5, stock6, stock7, stock8, stock9)
stock <- stock.raw
stock <- as.data.table(stock)
setnames(stock, c("Trddt", "Stkcd"), c("date", "stock.id"))
stock <- stock[, c("stock.id", "date", "Clsprc", "Dretwd", "Dsmvtll")]

# 这里有过save了
save(stock,file = "stock.RData")


load("stock.RData")
#整理
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, date := as.Date(date)]

# 计算stock的β
data.stock <- stock
data.stock <- data.stock[order(stock.id, date)]
# 匹配riskfree
data.stock <- riskfree[data.stock, on = .(date), nomatch = 0]
#匹配marketrisk
data.stock <- marketrisk[data.stock, on = .(date), nomatch = 0]

data.stock <- data.stock[, riskpreimum := market_return - risk_free.daily
	][, rit := Dretwd - risk_free.daily]

# 回归残差 stock.beta
data.stock <- data.stock[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, stock.beta := coef(lm(rit ~ riskpreimum))[2], keyby = .(stock.id, year, sem)
	][, num.day := .N, keyby = .(stock.id, year, sem)]

data.stockQ <- data.stock

#删除重复项(也就是daily变成sem)
data.stockQ <- data.stockQ[, yesno := duplicated(stock.beta), keyby= .(stock.id, sem, year)
	][yesno == FALSE, .SD
	][, c("stock.id", "year", "sem", "stock.beta", "num.day")]

save(data.stockQ,file = "stock-beta.RData")

# 加入基金portfolio
load("fund-portfolio.RData")
load("stock-beta.RData")
data <- portfolio
data <- data[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2]

data <- data.stockQ[data, on = .(stock.id, year, sem)]

# 暂时先忽略股票beta的异常值吧，具体可看stock-beta.RData，问题有点多的，包括想了新股的办法，想了计算天数的办法，但是依旧会存在问题
data <- data[, cj := proportion.stock * stock.beta
	][, wa.beta := sum(cj, na.rm = TRUE), keyby = .(id, year, sem)]

# 变成半年度
data <- data[, yesno := duplicated(wa.beta), keyby = . (id, sem, year)
	][yesno == FALSE, .SD
	][, c("id", "year", "sem", "wa.beta")]

zh.beta <- data
load("fund3.RData")

fund.3 <- zh.beta[fund.3, on = .(id, year, sem)]
# 很多基金2017年报持仓都没有出来 719个NA

fund.4 <- fund.3
save(fund.4, file = "fund4.RData")





# 这里是计算average-weighted beta
# 从data.stockQ 出发，试了上市日期，也没太大用处
load("stock-beta.RData")

intomarket <- read_excel("C://Users//shenfan//Desktop//data//2//股票上市日期.xlsx")
intomarket <- as.data.table(intomarket)
setnames(intomarket, 1:3, c("code", "name", "M.date"))
intomarket <- intomarket[code != "NA", .SD
	][code != "数据来源：Wind", .SD
	][, stock.id := substring(code, 1, 6)
	][, M.date := as.Date(M.date)
	][, year := year(M.date)
	][, quarter := quarter(M.date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, intomarket := 1
	][, c("code", "name", "quarter", "M.date") := NULL]

data.stockQ <- intomarket[data.stockQ, on = .(stock.id, year, sem)]
