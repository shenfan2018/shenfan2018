# table 3
load("sample.RData")

table <- stock.p[, .(stock.id, year, month, hy, num.name, ret)
	][month == 1 | month == 2 | month == 3, quarter := 1
	][month == 4 | month == 5 | month == 6, quarter := 2
	][month == 7 | month == 8 | month == 9, quarter := 3
	][month == 10 | month == 11 | month == 12, quarter := 4]

time <- c("2003-03-31", "2003-06-30", "2003-09-30", "2003-12-31", "2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30", "2018-09-30", "2018-12-31")

# 加上机构持股比例合计
IO <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//机构持股比例合计.xlsx")
IO <- as.data.table(IO)
setnames(IO, 1:66, c("code", "name", time))
IO <- IO[!is.na(name)]
IO <- melt(IO, id.vars = c("code", "name"), measure.vars = time)
setnames(IO, c("value", "variable"), c("IO", "date"))

IO <- IO[, date := as.Date(as.character(date))
	][, name := NULL
	][order(code, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, quarter, IO)]
# match
table <- IO[table, on = .(year, quarter, stock.id)]

# 加上股东户数/size
holders <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//股东户数.xlsx")
holders <- as.data.table(holders)
setnames(holders, 1:66, c("code", "name", time))
holders <- melt(holders, id.vars = c("code", "name"), measure.vars = time)
setnames(holders, c("value", "variable"), c("holders", "date"))
holders <- holders[, stock.id := substring(code, 1, 6)
	][, date := as.Date(as.character(date))
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, year, quarter, holders)]

# 流通市值
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund/stock.RData")
size <- stock[, year := year(date)
	][, quarter := quarter(date)
	][order(stock.id, date)
	][, .SD[.N], keyby = .(stock.id, year, quarter)
	][, .(stock.id, year, quarter, Dsmvosd)] 

# holders/size
holder <- holders[size, on = .(stock.id, year, quarter)
	][, holder := holders / (Dsmvosd / 1e3)
	][, .(stock.id, year, quarter, holder)]

# match
table <- holder[table, on = .(year, quarter, stock.id)]

# 分析师
ANA1 <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//分析师//分析师预测指标文件2003.1.1-2011.1.1//AF_Forecast.xlsx")
ANA2 <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//分析师//分析师预测指标文件2011.1.1-2019.1.1//AF_Forecast.xlsx")
ANA <- rbindlist(list(ANA1, ANA2))
rm(ANA1, ANA2)
setnames(ANA, 1:4, c("date", "id", "name", "stock.id"))
ANA <- ANA[stock.id != "证券代码'"
	][stock.id != "没有单位'"
	][, date := as.Date(date)
	][order(stock.id, date)
	][, year := year(date)
	][, month := month(date)
	][, .(ANA = (.N)), keyby = .(stock.id, year, month)]
# match
table <- ANA[table, on = .(stock.id, year, month)]

# AQ (没有按照fama macbeth数据结构)
AQ <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/yubin/AQ.csv")
AQ <- AQ[, Stkcd := as.character(Stkcd)
	][, stock.id := str_pad(Stkcd, 6, side = "left", pad = "0")
	][, .(stock.id, year, AQ)]

table <- AQ[table, on = .(stock.id, year)]

save(table, file = "IOANA.RData")

################################################################
load("IOANA.RData")
# 加入牛熊
# 各种因子以及alpha 
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
# match 牛为1，熊为0
table <- four[table, on = .(year, month)]
table <- table[, mktrf := ifelse(mkt_rf > 0, 1, 0)]

table <- table[, ret.2 := shift(ret, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)
	][, .(stock.id, year, month, mktrf, num.name, ret, ret.2, IO, ANA, holder, AQ)]
table <- table[, lapply(colnames(table[, 8:11]), str_c, ".g") %>% unlist() := lapply(.SD[, 5:8], ntile, 5), keyby = .(year, month, num.name)
	][, - (8:11)]

table = melt(table, id.vars = 1:7, measure.vars = 8:11)

table <- table[!is.na(value)
	][, .(ret = mean(ret, na.rm = TRUE)), keyby = .(year, month, num.name, variable, value, mktrf)
	][!is.nan(ret)]

# mean
a <- table[, .(ret = mean(ret)), keyby = .(mktrf, num.name, variable, value)]
write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference
a <- table[order(variable, year, month, num.name, value)
	][, ret2 := shift(ret, n = 5, fill = NA, type = "lead"), keyby = .(variable)
	][num.name == 3
	][, dif := ret - ret2
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(variable, value)]
write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")



############################################################################################################################################################################################################################################ 用fama macbeth数据格式，分牛熊，并且数据从简，变成年度数据
# 加上机构持股比例合计
IO <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//机构持股比例合计.xlsx")
IO <- as.data.table(IO)
setnames(IO, 1:66, c("code", "name", time))
IO <- IO[!is.na(name)]
IO <- melt(IO, id.vars = c("code", "name"), measure.vars = time)
setnames(IO, c("value", "variable"), c("IO", "date"))

IO <- IO[, date := as.Date(as.character(date))
	][, name := NULL
	][order(code, date)
	][, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(IO = mean(IO, na.rm = TRUE)), keyby = .(stock.id, year)]
# match
table <- IO

# 加上股东户数/size
holders <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//股东户数.xlsx")
holders <- as.data.table(holders)
setnames(holders, 1:66, c("code", "name", time))
holders <- melt(holders, id.vars = c("code", "name"), measure.vars = time)
setnames(holders, c("value", "variable"), c("holders", "date"))
holders <- holders[, stock.id := substring(code, 1, 6)
	][, date := as.Date(as.character(date))
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, year, quarter, holders)]

# 流通市值
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund/stock.RData")
size <- stock[, year := year(date)
	][, quarter := quarter(date)
	][order(stock.id, date)
	][, .SD[.N], keyby = .(stock.id, year, quarter)
	][, .(stock.id, year, quarter, Dsmvosd)]

# holders/size
holder <- holders[size, on = .(stock.id, year, quarter)
	][, holder := holders / (Dsmvosd / 1e3)
	][, .(holder = mean(holder, na.rm = TRUE)), keyby = .(stock.id, year)]
# match
table <- holder[table, on = .(stock.id, year)]

# 分析师
ANA1 <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//分析师//分析师预测指标文件2003.1.1-2011.1.1//AF_Forecast.xlsx")
ANA2 <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//分析师//分析师预测指标文件2011.1.1-2019.1.1//AF_Forecast.xlsx")
ANA <- rbindlist(list(ANA1, ANA2))
rm(ANA1, ANA2)
setnames(ANA, 1:4, c("date", "id", "name", "stock.id"))
ANA <- ANA[stock.id != "证券代码'"
	][stock.id != "没有单位'"
	][, date := as.Date(date)
	][order(stock.id, date)
	][, year := year(date)
	][, .(ANA = (.N)), keyby = .(stock.id, year)]
# match
table <- ANA[table, on = .(stock.id, year)]

# AQ (没有按照fama macbeth数据结构)
AQ <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/yubin/AQ.csv")
AQ <- AQ[, Stkcd := as.character(Stkcd)
	][, stock.id := str_pad(Stkcd, 6, side = "left", pad = "0")
	][, .(stock.id, year, AQ)]

table <- AQ[table, on = .(stock.id, year)]

setnames(table, "year", "year.match")

# 加入
load("sample.RData")
# year.match 2017匹配2018.7至2019.6
data <- stock.p[, .(stock.id, num.name, year, month, ret)
	][, year.match := ifelse(month > 6, year - 1, year - 2)]

# 加入牛熊
# 各种因子以及alpha 
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
four <- four[, .(year, month, mkt_rf)]
# match 牛为1，熊为0
data <- four[data, on = .(year, month)]
data <- data[, mktrf := ifelse(mkt_rf > 0, 1, 0)]

table <- table[data, on = .(stock.id, year.match)]

save(table, file = "charcterFAMA.RData")

# 这里先不赋予0 ANA,IO
load('charcterFAMA.RData')

table <- table[, lapply(colnames(table[, 3:6]), str_c, ".g") %>% unlist() := lapply(.SD[, 3:6], ntile, 5), keyby = .(year, month, num.name)
	][, .(stock.id, num.name, year, month, mktrf, ret, IO.g, ANA.g, holder.g, AQ.g)]

table = melt(table, id.vars = 1:6, measure.vars = 7:10)

table <- table[!is.na(value)
	][, .(ret = mean(ret)), keyby = .(year, month, mktrf, num.name, variable, value)]

# mean
a <- table[, .(ret = mean(ret)), keyby = .(mktrf, num.name, variable, value)]
write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference
a <- table[order(variable, year, month, num.name, value)
	][, ret2 := shift(ret, n = 5, fill = NA, type = "lead"), keyby = .(variable)
	][num.name == 3
	][, dif := ret - ret2
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(mktrf, variable, value)]
write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")


# add 其他








## IO
#biao <- table
#biao <- biao[, IO := ifelse(is.na(IO), 0, IO)
	#][, IO.tb := ntile(IO, 5), keyby = .(year, month, num.name)]

#biao <- biao[!is.na(IO.tb)
	#][, .(ret = mean(ret)), keyby = .(year, month, num.name, IO.tb)]
## mean
#a <- biao[, .(ret = mean(ret)), keyby = .(num.name, IO.tb)]
#write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")
## difference
#a <- biao[, ret2 := shift(ret, n = 5, fill = NA, type = "lead")
	#][num.name == 3
	#][, dif := ret - ret2
	#][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(IO.tb)]
#write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# analysis
biao <- table
biao <- biao[, ANA.tb := ntile(ANA, 5), keyby = .(year, month, num.name)]
# [, ANA := ifelse(is.na(ANA), 0, ANA)]

biao <- biao[!is.na(ANA.tb)
	][, .(ret = mean(ret)), keyby = .(year, month, num.name, ANA.tb)]
# mean
a <- biao[, .(ret = mean(ret)), keyby = .(num.name, ANA.tb)]
write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")
# difference
a <- biao[, ret2 := shift(ret, n = 5, fill = NA, type = "lead")
	][num.name == 3
	][, dif := ret - ret2
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(ANA.tb)]
write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")


################################################################################ add more
# 孙老师
sun <- fread("C:/Users/shenfan/Desktop/prospect value/firm_chara.csv")
sun <- sun[, c("qualified", "Markettype") := NULL
	][, stkcd := as.character(stkcd)
	][, stkcd := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year := year - 1]
setnames(sun, c("stkcd"), c("stock.id"))

# match
table <- sun[table, on = .(stock.id, year)]

# table 3
load("sample.RData")
