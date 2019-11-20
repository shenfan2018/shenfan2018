load("stock20030101-20190331.RData")

addstock <- read_excel("C:/Users/shenfan/Desktop/Buffett alpha/日个股回报率文件20190401-20190410/TRD_Dalyr.xlsx")
addstock<-as.data.table(addstock)
setnames(addstock, c("Trddt", "Stkcd"), c("date", "stock.id"))
addstock <- addstock[, c("stock.id", "date", "Clsprc", "Dretwd", "Dsmvtll")]
addstock <- addstock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, date := as.Date(date)
	][, Dsmvtll := as.numeric(Dsmvtll)]

stock.new <- rbindlist(list(stock, addstock)) #stock原文件 勿删

# A 股
A <- read_excel("C:/Users/shenfan/Desktop/Buffett alpha/A股.xlsx")
A <- as.data.table(A)
setnames(A, 1:2, c("code", "name"))
A <- A[, stock.id := substring(code, 1, 6)
	][, .(stock.id, name)]

# 去年和今年
data <- stock.new
data <- data[date > as.Date("2017-12-31")
	][, year := year(date)
	][, .(max = max(Clsprc)), keyby = .(stock.id, year)]

data <- A[data, on = .(stock.id), nomatch = 0
	][, max2019 := shift(max, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)]
setnames(data, "max", "max2018")

data <- data[!is.na(max2019)
	][, high := ifelse(max2019 > max2018, 1, 0)]

sum(data[, high]) / data[, .N]

# 历史新高
data <- stock.new
data <- data[, year := year(date)
	][, .(max = max(Clsprc)), keyby = .(stock.id, year)
	][year < 2019
	][, .(maxbefore = max(max)), keyby = .(stock.id)]

data.2019 <- stock.new
data.2019 <- data.2019[, year := year(date)
	][year == 2019
	][, .(max2019 = max(Clsprc)), keyby = .(stock.id)]

data <- data.2019[data, on = .(stock.id)
	][, high := ifelse(max2019 > maxbefore, 1, 0)]

data <- A[data, on = .(stock.id), nomatch = 0
	][!is.na(max2019)]

sum(data[, high]) / data[, .N]






