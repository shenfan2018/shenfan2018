load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, year, sem, stock.id, Shares)]

# ���л���
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
fund <- portfolio[order(id)
	][, .(id)
	][, .(id = unique(id))]
fund <- fund[["id"]]

date <- read_excel("C:/Users/shenfan/Desktop/�籣/data/ʱ��.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, month := month(date)
	][month == 6 | month == 12
	][date < as.Date("2018-01-01")
	][, .(date)]
date <- date[["date"]]

# ����stock
A <- read_excel("C:/Users/shenfan/Desktop/Buffett alpha/A��.xlsx")
A <- as.data.table(A)
setnames(A, 1:2, c("code", "name"))
A <- A[, stock.id := substring(code, 1, 6)
	][, .(stock.id)]
A <- A[["stock.id"]]

#perfect ������ƴ
data <- CJ(id = fund, stock.id = A, date = date)

data <- data[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# pfonly ÿ����������Ĺ�Ʊ��ѡ����
pfonly <- pf[order(id, stock.id)
	][, .(stock.id = unique(stock.id)), keyby = .(id)]

data <- pfonly[data, on = .(id, stock.id), nomatch = 0]

# ƥ����ʵ��
data <- pf[data, on = .(id, stock.id, year, sem)]


# �������Ķ��ǵ��ڣ�Ҳ���ǵ��ڼӲֻ��ǵ��ڼ���
# ����ǰ��û�����
syf <- data[, Shares.1 := shift(Shares, n = 1, fill = NA, type = "lag"), keyby = .(id, stock.id)
	][, buy1 := ifelse(is.na(Shares.1) & !is.na(Shares), 1, 0)]

# �Ӳ֣� ���ڱ�֮ǰ��Ķࣨǰ�������ڶ���ģ�
syf <- syf[, buy2 := ifelse( Shares > Shares.1, 1, 0)]

# ������Ҳ������һ�ھ�û����
syf <- syf[, sell1 := ifelse(is.na(Shares) & !is.na(Shares.1), 1, 0)]

# ���֣����ڱ�֮ǰ����
syf <- syf[, sell2 := ifelse(Shares < Shares.1, 1, 0)]

# ����
syf <- syf[, buy := ifelse(!is.na(buy2), buy2, buy1)
	][, sell := ifelse(!is.na(sell2), sell2, sell1)
	][, .(id, stock.id, date, year, sem, Shares, buy, sell)
	][buy == 1 | sell == 1]


# high TK �Ļ���
fund.TK <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/fund-returnwithTK20190328.xlsx")
fund.TK <- as.data.table(fund.TK)
# ����char���numeric
fund.TK <- fund.TK[, colnames(fund.TK[, 7:18]) := lapply(.SD[, 7:18], as.numeric)
	][, date := as.Date(date)]

# id������
fund.TK <- fund.TK[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")]

fund.TK <- fund.TK[, - (11:18)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# sem ����ѡ����10%
fund.TK <- fund.TK[!is.nan(TK)
	][, .(TK = mean(TK)), keyby = .(id, year, sem)
	][, TK.g := ntile(TK, 5), keyby = .(year, sem)
	][TK.g == 5]

highTKF <- fund.TK[syf, on = .(id, year, sem), nomatch = 0]

############################################################
# ����buy��stock
buy.stock <- highTKF[buy == 1
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]

# ����sell��stock
sell.stock <- highTKF[sell == 1
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]



# buy
buy.stock <- highTKF[buy == 1
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]

load("stocks.RData")
stock.s <- stock.s[, MV := log(MV)]
buy.stock <- buy.stock[stock.s, on = .(stock.id, year, sem), nomatch = 0
	][order(year, sem, stock.id)
	][, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE)), keyby = .(year, sem)]

buy <- buy.stock[, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE))]

# sell
sell.stock <- highTKF[sell == 1
	][, .(stock.id = unique(stock.id)), keyby = .(year, sem)]

sell.stock <- sell.stock[stock.s, on = .(stock.id, year, sem), nomatch = 0
	][order(year, sem, stock.id)
	][, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE)), keyby = .(year, sem)]

sell <- sell.stock[, .(MV = mean(MV, na.rm = TRUE), BM = mean(BM, na.rm = TRUE), GP = mean(GP, na.rm = TRUE), stockTK = mean(stockTK, na.rm = TRUE), max = mean(max, na.rm = TRUE), skewness = mean(skewness, na.rm = TRUE), semi_ret = mean(semi_ret, na.rm = TRUE))]

# dif
dif <- sell.stock[buy.stock, on = .(year, sem)
	][, .(MV.d = i.MV - MV, BM.d = i.BM - BM, GP.d = i.GP - GP, stockTK.d = i.stockTK - stockTK, max.d = i.max - max, skewness.d = i.skewness - skewness, semi_ret.d = i.semi_ret - semi_ret)]

t.test(dif[, stockTK.d])

fit <- lm(stockTK.d ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)
