# 龙虎榜  玩玩 2010.1.1-2018.12.31
lhb <- read_excel("C://Users//shenfan//Desktop//社保//data//营业部交易披露明细.xlsx")
lhb <- as.data.table(lhb)
setnames(lhb, 1:4, c("code", "name", "end", "start"))
setnames(lhb, 7:9, c("yyb", "buy", "sell"))
lhb <- lhb[order(start, code)
	][, end := as.Date(as.character(end))
	][, year := year(end)]

# 仅保留是A股的
A <- read_excel("C://Users//shenfan//Desktop//社保//data//全部A股.xlsx")
A <- as.data.table(A)
setnames(A, 1, "code")
A <- A[, stock.id := substring(code, 1, 6)
	][, .(stock.id, code)]
lhb <- A[lhb, on = .(code), nomatch = 0]


# 加入stock
# 股价整理 为2010-2017的日度数据(2003.1.1-2018.12.31) stocklhd
load("stocklhb.RData")
stock <- stock[order(stock.id, date)]

for (i in 1:20) {
	stock <- stock[, str_c(colnames(stock)[3], str_c(".h", i)) := lapply(.SD[, 2], shift, n = i, fill = NA, type = "lead"), keyby = stock.id
	]
}

stock <- stock[, str_c("Dretwd.h", seq(1:20)) := .SD[, 6:25] / Clsprc - 1]
setnames(stock, "date", "end")
a <- stock[, - (3:25)]

# match到龙虎榜 买入的股票 未来20个交易日的累计收益
lhb <- a[lhb, on = .(end, stock.id), nomatch = 0]

## 区分是买还是卖还是都操作，这里若买入>卖出，则归为买入,cz==1为买，cz==0为卖
a <- lhb[is.na(buy), cz := 0
	][is.na(sell), cz := 1
	][is.na(cz), cz := ifelse(buy > sell, 1, 0)]

## 一年一归类，总前提都是买的，看入围次数，高入围出现次数(num),高胜率，后一个交易日上涨的概率(sl)，高收益(sy)
tj <- a[cz == 1
	][, sz.h1 := ifelse(Dretwd.h1 > 0, 1, 0)
	][order(year, yyb)
	][, .(num = (.N), sl = sum(sz.h1, na.rm = TRUE) / (.N), sy = mean(Dretwd.h1, na.rm = TRUE)), keyby = .(year, yyb)
	][num > 20
	][order(year, - num)
	][, num.pm := seq(1:(.N)), keyby = year
	][order(year, - sl)
	][, sl.pm := seq(1:(.N)), keyby = year
	][order(year, - sy)
	][, sy.pm := seq(1:(.N)), keyby = year
	][, zh.pm := num.pm + sl.pm + sy.pm
	][order(-year, zh.pm)]



## 高胜率
sl <- lhb[, sz.h1 := ifelse(Dretwd.h1 > 0, 1, 0)
	][order(year, yyb)
	][, .(sl.h1 = sum(sz.h1,na.rm = TRUE) / (.N), ruwei = (.N)), keyby = .(year, yyb)
	][order(year, - sl.h1)
	][ruwei > 20
	][, pm := seq(1:(.N)), keyby = year
	][pm < 21 ]

## 高收益
sy <- lhb[order(year, yyb)
	][, .(sy.h1 = mean(Dretwd.h1, na.rm = TRUE), ruwei = (.N)), keyby = .(year, yyb)
	][order(year, - sy.h1)
	][ruwei > 20
	][, pm := seq(1:(.N)), keyby = year
	][pm < 21]









a <- lhb[, buy.yes := ifelse(is.na(buy), 0, 1)
	][, sell.yes := ifelse(is.na(sell), 0, 1)
	][order(year, yyb)
	][, .(sbs = (.N)), keyby = .(year, yyb)
	][order(-year, - sbs)]

# 特力A 
b <- lhb[yyb == "华泰证券股份有限公司厦门厦禾路证券营业部"]

# 提取了什么证券
c <- lhb[, zqgs := str_replace_all(yyb, "证券营业部", "")
	][, zqgs := str_extract(zqgs, "..证券")]

d <- c[, .(sbs = (.N)), keyby = .(year, zqgs)
	][order(-year, - sbs)]

b <- tj[yyb == "华泰证券股份有限公司厦门厦禾路证券营业部"]