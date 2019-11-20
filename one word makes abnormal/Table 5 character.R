## 用fama macbeth数据格式，分牛熊，并且数据从简，变成年度数据
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


# add sun
sun <- fread("C:/Users/shenfan/Desktop/prospect value/firm_chara.csv")
sun <- sun[, c("qualified", "Markettype") := NULL
	][, stkcd := as.character(stkcd)
	][, stkcd := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year := year - 1]
setnames(sun, c("stkcd"), c("stock.id"))

# match
table <- sun[table, on = .(stock.id, year)]

#  match stock
load("sample.RData")
data <- stock.p[, .(stock.id, hy, num.name, year, month, ret)]

# 加入牛熊
# 各种因子以及alpha 
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
four <- four[, .(year, month, mkt_rf, rf)]
# match 牛为1，熊为0
data <- four[data, on = .(year, month)]
data <- data[, mktrf := ifelse(mkt_rf > 0, 1, 0)]

data <- data[, .(stock.id, hy, num.name, year, month, ret, rf, mkt_rf, mktrf)
	][, year.match := ifelse(month > 4, year - 1, year - 2)]

# final match
setnames(table, "year", "year.match")
data <- table[data, on = .(stock.id, year.match)]

write.csv(data, "C://Users//shenfan//Desktop//onewordtable5.csv")