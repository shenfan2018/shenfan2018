stock1 <- read_excel("C:/Users/shenfan/Desktop/可转债/日个股回报率文件/TRD_Dalyr.xlsx")
stock2 <- read_excel("C:/Users/shenfan/Desktop/可转债/日个股回报率文件/TRD_Dalyr1.xlsx")
stock3 <- read_excel("C:/Users/shenfan/Desktop/可转债/日个股回报率文件/TRD_Dalyr2.xlsx")

stock.raw <- rbindlist(list(stock1, stock2, stock3)) #stock原文件 勿删
rm(stock1, stock2, stock3)

stock <- stock.raw
setnames(stock, 1:4, c("stock.id", "date", "open", "close"))

stock <- stock[!stock.id == "证券代码'"
	][!stock.id == "没有单位'"
	][, open := as.numeric(open)
	][, close := as.numeric(close)
	][, date := as.Date(date)]

stock <- stock[order(stock.id, date)
	][, open.1 := shift(open, n = 1, fill = NA, type = "lead"), keyby = stock.id
	][, ret.stock := (open.1 - close) / close
	][, .(stock.id, date, close, ret.stock)]

# bond ret
bond <- read_excel("C:/Users/shenfan/Desktop/可转债/上市表现.xlsx")
bond <- as.data.table(bond)
bond <- bond[, c("上市日期", "代码",  "上市首日收盘涨跌幅(%)")]
setnames(bond, 1:3, c("launch.date", "bond.code",  "ret.bond"))
bond <- bond[!is.na(bond.code)
	][, ret.bond := ret.bond / 100
	][, bond.id := substring(bond.code, 1, 6)
	][, .(bond.id, launch.date, ret.bond)]

# bond 股权登记日
bond2 <- read_excel("C:/Users/shenfan/Desktop/可转债/bond.xlsx")
bond2 <- as.data.table(bond2)
bond2 <- bond2[, c("债券代码", "债券简称", "申购日期", "正股代码", "每股配售额     ")]
setnames(bond2, 1:5, c("bond.id", "bond.name", "subscribe.date", "stock.id", "allocation"))
bond2 <- bond2[, subscribe.date := substring(subscribe.date, 1, 10)
	][, subscribe.date := as.Date(subscribe.date)
	][, stock.id := as.character(stock.id)
	][, stock.id := str_pad(stock.id, 6, side = "left", pad = "0")
	][, allocation := as.numeric(allocation)
	][, bond.id := as.character(bond.id)]

# 实际上，股权登记日为申购日期的前一天，但是必须是交易日
date <- stock[, .(subscribe.date = unique(date))
	][, allotment.date := shift(subscribe.date, n = 1, fill = NA, type = "lag")]

# match
bond2 <- date[bond2, on = .(subscribe.date)]

# 把stock date改成股权登记日那天
setnames(stock, "date", "allotment.date")

# 综合 主表为data
data <- bond2
data <- bond[data, on = .(bond.id)]
data <- stock[data, on = .(stock.id, allotment.date)]

setnames(data, "allotment.date", "date.of.record")

data <- data[, .(bond.id, bond.name, subscribe.date, date.of.record, allocation, stock.id, ret.stock, close, launch.date, ret.bond)]

save(data, file = "bond.RData")

###############################################
load("bond.RData")

a <- data
a <- a[, year := year(launch.date)
	][, .N, keyby = .(year)
	][!is.na(year)]


ggplot(a, aes(x = year)) +
	geom_bar(aes(y = N), stat = "identity") +
	geom_text(data = a, aes(x = year, y = N, label = N), stat = "identity", vjust = -0.5) +
	scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
	theme_bw() +
#	scale_x_discrete(labels = c("2007", "2009", "2011", "2013", "2015", "2017", "2019")) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 1),
#	legend.title = element_blank(),
#	legend.position = c(0.15, 0.85),

)

##############
load("bond.RData")
data <- data[subscribe.date > as.Date("2017-01-01")
	][!is.na(launch.date)
	][order(subscribe.date)]

### difference
a <- data[, .(bond.id, bond.name, allotment.date, launch.date)
	][, launch.date := as.Date(launch.date)
	][, days := difftime(launch.date, allotment.date, units = c("days"))]
setnames(a, 1:5, c("债券代码", "债券名称", "股权登记日", "上市时间", "相隔时间"))


# 先做一个每一笔的图
fig <- data[, .(bond.id, subscribe.date, ret.stock, close, allocation, ret.bond)]

ggplot(fig, aes(x = subscribe.date)) +
	geom_point(aes(y = ret.stock), colour = "blue", stat = "identity") +
	geom_point(aes(y = ret.bond), colour = "red", stat = "identity") +
#	geom_text(data = fig, aes(x = subscribe.date, y = bond.id, label = bond.id), stat = "identity", vjust = -0.5) +
#	scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
	theme_bw() +
	#	scale_x_discrete(labels = c("2007", "2009", "2011", "2013", "2015", "2017", "2019")) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 1),
#	legend.title = element_blank(),
#	legend.position = c(0.15, 0.85),

)


# 转债因子调整 先做一个每一笔的图
fig <- data[, .(bond.id, subscribe.date, ret.stock, close, allocation, ret.bond)
	][, ret.bond2 := 1 / close * allocation * ret.bond]

ggplot(fig, aes(x = subscribe.date)) +
	geom_point(aes(y = ret.stock), colour = "blue", stat = "identity") +
	geom_point(aes(y = ret.bond2), colour = "green", stat = "identity") +
	#	geom_text(data = fig, aes(x = subscribe.date, y = bond.id, label = bond.id), stat = "identity", vjust = -0.5) +
	#	scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
	theme_bw() +
	#	scale_x_discrete(labels = c("2007", "2009", "2011", "2013", "2015", "2017", "2019")) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 1),
#	legend.title = element_blank(),
#	legend.position = c(0.15, 0.85),

)

################################################
## 滚动
load("bond.RData")
data <- data[subscribe.date > as.Date("2017-01-01")
	][!is.na(launch.date)
	][order(subscribe.date)]
ret <- data[, .(bond.id, subscribe.date, ret.stock)]
ret <- ret[order(subscribe.date)
	][, tag := seq(1:(.N))
	][, ret.stock2 := ret.stock + 1]

syf <- vector()
for (i in 1:138) {
	syf[[i]] <- ret[tag >= 1 & tag <= i, {
		I <- prod(ret.stock2) - 1
	},
	]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

setnames(ret, "syf", "cum.ret.s")
ret <- ret[, .(bond.id, cum.ret.s)]

data <- ret[data, on = .(bond.id)]

# 现在的市值，先粗略
a <- data
a <- a[, hold.s := (1 + cum.ret.s) * 1000000
	][, hold.s.1 := shift(hold.s, n =1 , fill = NA, type = "lag")
	][, hold.s.1 := ifelse(is.na(hold.s.1), 1000000, hold.s.1)
	][, hold.b := hold.s.1 / close * allocation
	][, bond.win := hold.b * ret.bond
	][, stock.win := hold.s.1 * ret.stock
	][, final.win := bond.win + stock.win
	][, ret := final.win / 1000000
	]

ret <- a[, .(bond.id, subscribe.date, ret)]
ret <- ret[order(subscribe.date)
	][, tag := seq(1:(.N))
	][, ret2 := ret + 1]

syf <- vector()
for (i in 1:138) {
	syf[[i]] <- ret[tag >= 1 & tag <= i, {
		I <- prod(ret2) - 1
	},
	]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

setnames(ret, "syf", "cum.ret")
ret <- ret[, .(bond.id, cum.ret)]

a <- ret[a, on = .(bond.id)]

ggplot(a, aes(x = date.of.record)) +
	geom_line(aes(y = cum.ret.s)) +
#	geom_text(data = a, aes(x = year, y = N, label = N), stat = "identity", vjust = -0.5) +
#	scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
	theme_bw() +
	#	scale_x_discrete(labels = c("2007", "2009", "2011", "2013", "2015", "2017", "2019")) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 1),
#	legend.title = element_blank(),
#	legend.position = c(0.15, 0.85),

)


###################################################
# 方案1：每次正股都按照100w进行
load("bond.RData")
data <- data[subscribe.date > as.Date("2017-01-01")
	][!is.na(launch.date)
	][order(subscribe.date)]
a <- data
a <- a[, hold.s := 1000000
	][, hold.b := 1000000 / close * allocation
	][, bond.win := hold.b * ret.bond
	][, stock.win := hold.s * ret.stock
	][, final.win := bond.win + stock.win
	][, ret := final.win / 1000000
	]

ret <- a[, .(bond.id, subscribe.date, ret)]
ret <- ret[order(subscribe.date)
	][, tag := seq(1:(.N))
	][, ret2 := ret + 1]

syf <- vector()
for (i in 1:138) {
	syf[[i]] <- ret[tag >= 1 & tag <= i, {
		I <- prod(ret2) - 1
	},
	]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

setnames(ret, "syf", "cum.ret")
ret <- ret[, .(bond.id, cum.ret)]

a <- ret[a, on = .(bond.id)]

ggplot(a, aes(x = date.of.record)) +
	geom_line(aes(y = cum.ret)) +
	#	geom_text(data = a, aes(x = year, y = N, label = N), stat = "identity", vjust = -0.5) +
	#	scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
	theme_bw() +
	#	scale_x_discrete(labels = c("2007", "2009", "2011", "2013", "2015", "2017", "2019")) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 1),
#	legend.title = element_blank(),
#	legend.position = c(0.15, 0.85),

)


########################################################################
# 方案2 ：熊市我不管拉，我只管现在（2019年1月1日至今）
## 滚动
load("bond.RData")
data <- data[subscribe.date > as.Date("2019-01-01")
	][!is.na(launch.date)
	][order(subscribe.date)]
ret <- data[, .(bond.id, subscribe.date, ret.stock)]
ret <- ret[order(subscribe.date)
	][, tag := seq(1:(.N))
	][, ret.stock2 := ret.stock + 1]

syf <- vector()
for (i in 1:31) {
	syf[[i]] <- ret[tag >= 1 & tag <= i, {
		I <- prod(ret.stock2) - 1
	},
	]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

setnames(ret, "syf", "cum.ret.s")
ret <- ret[, .(bond.id, cum.ret.s)]

data <- ret[data, on = .(bond.id)]

# 现在的市值，先粗略
a <- data
a <- a[, hold.s := (1 + cum.ret.s) * 1000000
	][, hold.s.1 := shift(hold.s, n = 1, fill = NA, type = "lag")
	][, hold.s.1 := ifelse(is.na(hold.s.1), 1000000, hold.s.1)
	][, hold.b := hold.s.1 / close * allocation
	][, bond.win := hold.b * ret.bond
	][, stock.win := hold.s.1 * ret.stock
	][, final.win := bond.win + stock.win
	][, ret := final.win / 1000000
	]

ret <- a[, .(bond.id, subscribe.date, ret)]
ret <- ret[order(subscribe.date)
	][, tag := seq(1:(.N))
	][, ret2 := ret + 1]

syf <- vector()
for (i in 1:31) {
	syf[[i]] <- ret[tag >= 1 & tag <= i, {
		I <- prod(ret2) - 1
	},
	]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

setnames(ret, "syf", "cum.ret")
ret <- ret[, .(bond.id, cum.ret)]

a <- ret[a, on = .(bond.id)]

xbreaks <- c('2019-01-15', '2019-02-01', '2019-02-15', '2019-03-01', '2019-03-15', '2019-04-01', '2019-04-15')
ggplot(a, aes(x = date.of.record)) +
	geom_point(aes(y = cum.ret)) +
	#	geom_text(data = a, aes(x = year, y = N, label = N), stat = "identity", vjust = -0.5) +
	#	scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	#	scale_x_discrete(labels = c("2007", "2009", "2011", "2013", "2015", "2017", "2019")) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 1),
#	legend.title = element_blank(),
#	legend.position = c(0.15, 0.85),

)



########################################################################加入评级
rate <- read_excel("C:/Users/shenfan/Desktop/可转债/评级.xlsx")
rate <- as.data.table(rate)
setnames(rate, 2, "code")
setnames(rate, "评级结果", "rate")
rate <- rate[, bond.id := substring(code, 1, 6)
	][, .(bond.id, rate)
	][!is.na(bond.id)]

load("bond.RData")
data <- rate[data, on = .(bond.id)]

# AA及以上要
data <- data[rate == "AAA" | rate == "AA+" | rate == "AA" | rate == "AAA-"]

data <- data[subscribe.date > as.Date("2019-01-01")
	][!is.na(launch.date)
	][order(subscribe.date)]
ret <- data[, .(bond.id, subscribe.date, ret.stock)]
ret <- ret[order(subscribe.date)
	][, tag := seq(1:(.N))
	][, ret.stock2 := ret.stock + 1]

syf <- vector()
for (i in 1:138) {
	syf[[i]] <- ret[tag >= 1 & tag <= i, {
		I <- prod(ret.stock2) - 1
	},
	]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

setnames(ret, "syf", "cum.ret.s")
ret <- ret[, .(bond.id, cum.ret.s)]

data <- ret[data, on = .(bond.id)]

# 现在的市值，先粗略
a <- data
a <- a[, hold.s := (1 + cum.ret.s) * 1000000
	][, hold.s.1 := shift(hold.s, n = 1, fill = NA, type = "lag")
	][, hold.s.1 := ifelse(is.na(hold.s.1), 1000000, hold.s.1)
	][, hold.b := hold.s.1 / close * allocation
	][, bond.win := hold.b * ret.bond
	][, stock.win := hold.s.1 * ret.stock
	][, final.win := bond.win + stock.win
	][, ret := final.win / 1000000
	]

ret <- a[, .(bond.id, subscribe.date, ret)]
ret <- ret[order(subscribe.date)
	][, tag := seq(1:(.N))
	][, ret2 := ret + 1]

syf <- vector()
for (i in 1:138) {
	syf[[i]] <- ret[tag >= 1 & tag <= i, {
		I <- prod(ret2) - 1
	},
	]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

setnames(ret, "syf", "cum.ret")
ret <- ret[, .(bond.id, cum.ret)]

a <- ret[a, on = .(bond.id)]

xbreaks <- c('2019-01-15', '2019-02-01', '2019-02-15', '2019-03-01', '2019-03-15', '2019-04-01', '2019-04-15')
ggplot(a, aes(x = date.of.record)) +
	geom_line(aes(y = cum.ret)) +
	#	geom_text(data = a, aes(x = year, y = N, label = N), stat = "identity", vjust = -0.5) +
	#	scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	#	scale_x_discrete(labels = c("2007", "2009", "2011", "2013", "2015", "2017", "2019")) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 1),
#	legend.title = element_blank(),
#	legend.position = c(0.15, 0.85),

)


################################# 枪权
issue <- read_excel("C:/Users/shenfan/Desktop/可转债/评级.xlsx")
issue <- as.data.table(issue)
setnames(issue, 1:2, c("issuing.date", "code"))
issue <- issue[, bond.id := substring(code, 1, 6)
	][, .(bond.id, issuing.date)
	][, issuing.date := as.Date(issuing.date)
	][!is.na(bond.id)]

load("bond.RData")
data <- data[, .(bond.id, date.of.record, stock.id)]

data <- issue[data, on = .(bond.id)]

# stock
stock1 <- read_excel("C:/Users/shenfan/Desktop/可转债/日个股回报率文件/TRD_Dalyr.xlsx")
stock2 <- read_excel("C:/Users/shenfan/Desktop/可转债/日个股回报率文件/TRD_Dalyr1.xlsx")
stock3 <- read_excel("C:/Users/shenfan/Desktop/可转债/日个股回报率文件/TRD_Dalyr2.xlsx")

stock.raw <- rbindlist(list(stock1, stock2, stock3)) #stock原文件 勿删
rm(stock1, stock2, stock3)

stock <- stock.raw
setnames(stock, 1:4, c("stock.id", "date", "open", "close"))

stock <- stock[!stock.id == "证券代码'"
	][!stock.id == "没有单位'"
	][, open := as.numeric(open)
	][, close := as.numeric(close)
	][, date := as.Date(date)]

# 
setnames(stock, 2:4, c("date.of.record", "r.open", "r.close"))
data <- stock[data, on = .(date.of.record, stock.id)]
#
setnames(stock, 2:4, c("issuing.date", "i.open", "i.close"))
data <- stock[data, on = .(issuing.date, stock.id)]

# 处理
a <- data[!is.na(i.open)
	][issuing.date > as.Date("2017-01-01")
	][, ret := (r.close - i.open) / i.open
	][order(issuing.date)]

ret <- a[, .(bond.id, issuing.date, ret)
	][order(issuing.date)
	][, tag := seq(1:(.N))
	][, ret2 := ret + 1]

syf <- vector()
for (i in 1:151) {
	syf[[i]] <- ret[tag >= 1 & tag <= i, {
		I <- prod(ret2) - 1
	},
	]
}

syf <- as.data.table(syf)
syf <- syf[, tag := seq(1:(.N))]
ret <- syf[ret, on = .(tag)]

setnames(ret, "syf", "cum.ret")
ret <- ret[, .(bond.id, cum.ret)]

a <- ret[a, on = .(bond.id)]


ggplot(a, aes(x = date.of.record)) +
	geom_line(aes(y = cum.ret)) +
	#	geom_text(data = a, aes(x = year, y = N, label = N), stat = "identity", vjust = -0.5) +
	#	scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
	theme_bw() +
#	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	#	scale_x_discrete(labels = c("2007", "2009", "2011", "2013", "2015", "2017", "2019")) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 1),
#	legend.title = element_blank(),
#	legend.position = c(0.15, 0.85),

)

