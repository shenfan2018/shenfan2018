load("sample.RData")

data <- stock.p[, .(stock.id, year, month, hy, num.name, ret)]

# price (月平均，月度）
pri <- stock[, year := year(date)
	][, month := month(date)
	][, .(pri = mean(Clsprc, na.rm = TRUE)), keyby = .(stock.id, year, month)
	][, .(stock.id, year, month, pri)]
# match
data <- pri[data, on = .(stock.id, year, month)]
rm(pri)

# year2
data <- data[, year2 := ifelse(month > 6, year - 1, year - 2)]

time <- c("2003-06-30", "2004-06-30", "2005-06-30", "2006-06-30", "2007-06-30", "2008-06-30", "2009-06-30", "2010-06-30", "2011-06-30", "2012-06-30", "2013-06-30", "2014-06-30", "2015-06-30", "2016-06-30", "2017-06-30", "2018-06-30")
time3 <- c("2003-12-31", "2004-12-31", "2005-12-31", "2006-12-31", "2007-12-31", "2008-12-31", "2009-12-31", "2010-12-31", "2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31", "2018-12-31")
# size (6月底，股价*流通股）
size <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//流通市值.xlsx")
size <- as.data.table(size)
setnames(size, 1:18, c("code", "name", time))
size <- melt(size, id.vars = c("code", "name"), measure.vars = time)
setnames(size, c("value", "variable"), c("ltg", "date")) # ltg 流通股
size <- size[, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)]

size <- stock[size, on = .(stock.id, year, month), nomatch = 0]
size <- size[order(stock.id, date)
	][, .(size = unique(Clsprc[.N] * ltg)), keyby = .(stock.id, year, month)
	][, month := NULL]
setnames(size, "year", "year2")
# match
data <- size[data, on = .(stock.id, year2)]
rm(size)

time2 <- c("2003-03-31", "2003-06-30", "2003-09-30", "2003-12-31", "2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30", "2018-09-30", "2018-12-31")
# EMP: number of employees 6月的 员工数
EMP <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//员工数.xlsx")
EMP <- as.data.table(EMP)
setnames(EMP, 1:66, c("code", "name", time2))
EMP <- melt(EMP, id.vars = c("code", "name"), measure.vars = time2)
setnames(EMP, c("value", "variable"), c("EMP", "date"))
EMP <- EMP[, date := as.Date(as.character(date))
	][order(code, date)
	][, year := year(date)
	][, month := month(date)
	][, stock.id := substring(code, 1, 6)
	][month == 6, .(stock.id, year, EMP)]
setnames(EMP, "year", "year2")
# match
data <- EMP[data, on = .(stock.id, year2)]
rm(EMP)

# SH: number of shareholders 6月的 股东户数
SH <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//股东户数.xlsx")
SH <- as.data.table(SH)
setnames(SH, 1:66, c("code", "name", time2))
SH <- melt(SH, id.vars = c("code", "name"), measure.vars = time2)
setnames(SH, c("value", "variable"), c("SH", "date"))
SH <- SH[, date := as.Date(as.character(date))
	][order(code, date)
	][, year := year(date)
	][, month := month(date)
	][, stock.id := substring(code, 1, 6)
	][month == 6, .(stock.id, year, SH)]
setnames(SH, "year", "year2")
# match
data <- SH[data, on = .(stock.id, year2)]
rm(SH)

# IO: institutional investors 6月的 机构持股比例合计
IO <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//机构持股比例合计.xlsx")
IO <- as.data.table(IO)
setnames(IO, 1:66, c("code", "name", time2))
IO <- melt(IO, id.vars = c("code", "name"), measure.vars = time2)
setnames(IO, c("value", "variable"), c("IO", "date"))
IO<- IO[, date := as.Date(as.character(date))
	][order(code, date)
	][, year := year(date)
	][, month := month(date)
	][, stock.id := substring(code, 1, 6)
	][month == 6, .(stock.id, year, IO)]
setnames(IO, "year", "year2")
# match
data <- IO[data, on = .(stock.id, year2)]
rm(IO)

# trade day
trd <- stock
trd <- trd[order(stock.id, date)
	][, year := year(date)
	][, .(trd = (.N)), keyby = .(stock.id, year)]
setnames(trd, "year", "year2")
# match
data <- trd[data, on = .(stock.id, year2)]
rm(trd)

# Amihud指标（日）
load("liq.RData")
liq <- liq[, year := year(date)
	][, month := month(date)
	][, year2 := ifelse(month > 6, year, year - 1)
	][, .(amihud = mean(ILLIQ, na.rm = TRUE)), keyby = .(stock.id, year2)]
# match
data <- liq[data, on = .(stock.id, year2)]
rm(liq)

# GP GP: gross profitable 12月底的 毛利润/lag 总资产
GP <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//毛利润.xlsx")
GP <- as.data.table(GP)
setnames(GP, 1:18, c("code", "name", time3))
GP <- melt(GP, id.vars = c("code", "name"), measure.vars = time3)
setnames(GP, c("value", "variable"), c("mlr", "date"))
AS <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//资产总计.xlsx")
AS <- as.data.table(AS)
setnames(AS, 1:18, c("code", "name", time3))
AS <- melt(AS, id.vars = c("code", "name"), measure.vars = time3)
setnames(AS, c("value", "variable"), c("AS", "date"))
GP <- AS[GP, on = .(code, name, date)]

GP <- GP[!is.na(name)
	][, date := as.Date(as.character(date))
	][order(code, date)
	][, AS.1 := shift(AS, 1, fill = NA, type = 'lag'), keyby = code
	][, GP := mlr / AS.1
	][, stock.id := substring(code, 1, 6)
	][, year := year(date)
	][, .(stock.id, year, GP)]
setnames(GP, "year", "year2")
# match
data <- GP[data, on = .(stock.id, year2)]
rm(GP,AS)

# INV asset growth rate 12月
INV <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//总资产同比增长率.xlsx")
INV <- as.data.table(INV)
setnames(INV, 1:18, c("code", "name", time3))
INV <- melt(INV, id.vars = c("code", "name"), measure.vars = time3)
setnames(INV, c("value", "variable"), c("INV", "date"))
INV <- INV[, date := as.Date(as.character(date))
	][order(code, date)
	][, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, INV)]
setnames(INV, "year", "year2")
# match
data <- INV[data, on = .(stock.id, year2)]
rm(INV)

# BM 股东权益/流通市值
BM <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//股东权益合计 年报.xlsx")
BM <- as.data.table(BM)
setnames(BM, 1:18, c("code", "name", time3))
BM <- melt(BM, id.vars = c("code", "name"), measure.vars = time3)
setnames(BM, c("value", "variable"), c("equity", "date"))
BM <- BM[, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, equity)
	][order(stock.id, year)]
#流通股市值 12月底
MV <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//月个股流通市值//TRD_Mnth.xlsx")
MV <- as.data.table(MV)
setnames(MV, "Stkcd", "stock.id")
MV <- MV[month == 12
	][, MV := Msmvosd * 1000
	][, .(stock.id, year, MV)]
BM <- MV[BM, on = .(stock.id, year)
	][, BM := equity / MV
	][, .(stock.id, year, BM)]
setnames(BM, "year", "year2")
# match
data <- BM[data, on = .(stock.id, year2)]
rm(MV, BM)

# vol(去年6月到今年6月），每日的收益率的volatiltiy
vol <- stock
vol <- vol[, year2 := ifelse(month > 6, year, year - 1)
	][, .(vol = sd(Dretwd, na.rm = TRUE)), keyby = .(stock.id, year2)]
# match
data <- vol[data, on = .(stock.id, year2)]
rm(vol)

# ANA analysis 分析师 12月
ANA1 <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//分析师//分析师预测指标文件2003.1.1-2011.1.1//AF_Forecast.xlsx")
ANA2 <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//分析师//分析师预测指标文件2011.1.1-2019.1.1//AF_Forecast.xlsx")
ANA<- rbindlist(list(ANA1, ANA2)) 
rm(ANA1, ANA2)
setnames(ANA, 1:4, c("date", "id", "name","stock.id"))
ANA <- ANA[stock.id != "证券代码'"
	][stock.id!= "没有单位'"
	][, date := as.Date(date)
	][order(stock.id, date)
	][, year := year(date)
	][, .(ANA = (.N)), keyby = .(stock.id, year)]
setnames(ANA, "year", "year2")
# match
data <- ANA[data, on = .(stock.id, year2)]
rm(ANA)

# Float 可交易股票（流通股）percent 6月
float <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//流通股占总股本 6月.xlsx")
float <- as.data.table(float)
setnames(float, 1:18, c("code", "name", time))
float <- melt(float, id.vars = c("code", "name"), measure.vars = time)
setnames(float, c("value", "variable"), c("float", "date"))
float <- float[, date := as.Date(as.character(date))
	][order(code, date)
	][, year := year(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, float)]
setnames(float, "year", "year2")
# match
data <- float[data, on = .(stock.id, year2)]
rm(float)

# ret(-12,-2) 6月，去年7月到今年5月,这里month==7为去年7月至今年5月收益
ret <- stock
#先变月度
ret <- ret[order(stock.id, year, month)
	][, Dretwd2 := Dretwd + 1
	][, .(ret = prod(Dretwd2, na.rm = TRUE) - 1), keyby = .(stock.id, year, month)
	][, tag := seq(1:(.N)), keyby = stock.id
	][, ret2 := ret + 1]

reg.roll <- list()
for (i in 13:190) {
	reg.roll[[i]] <- ret[tag >= i - 12 & tag <= i-2, {
		I <- prod(ret2, na.rm = TRUE) %>% as.list()
	},
	keyby = .(stock.id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:190, reg.roll)
roll <- roll[13:190]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
ret <- reg.cof[ret, on = .(tag, stock.id)
	][, ret10 := V1 - 1
	][month == 7
	][, .(stock.id, year, ret10)]
setnames(ret, "year", "year2")

# match 
data <- ret[data, on = .(stock.id, year2)]
rm(ret, reg.cof, reg.roll, roll, i)

save(data, file = "variables.RData")

################################################################################### 孙老师
load("sample.RData")
data <- stock.p[, .(stock.id, year, month, hy, num.name, ret)]

# year2
data <- data[, year2 := ifelse(month > 6, year - 1, year - 2)]

# 孙老师
sun <- fread("C:/Users/shenfan/Desktop/prospect value/firm_chara.csv")
sun <- sun[, c("qualified", "Markettype") := NULL
	][, stkcd := as.character(stkcd)
	][, stkcd := str_pad(stkcd, 6, side = "left", pad = "0")]
setnames(sun, c("stkcd", "year"), c("stock.id", "year2"))

data <- sun[data, on = .(stock.id, year2)]

data <- data[, logsize := log(size)
	][, logbm := log(bm)
	][, namedummy := ifelse(num.name == 3, 1, 0)]

write.csv(data, "C://Users//shenfan//Desktop//onewordvariables.csv")