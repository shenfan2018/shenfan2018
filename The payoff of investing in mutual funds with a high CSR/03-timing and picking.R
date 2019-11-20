## beta ��ȥ12����
# �� ���ɹ�ȥ12���³��������ʻع鵽�г��ĳ���������
load("C:/Users/shenfan/source/repos/shenfan2018/Buffett alpha/stock20030101-20190331.RData")

# �������ݱ��monthly
stock <- stock[order(stock.id, date)
	][, month := month(date)
	][, year := year(date)]

stock.month <- stock[, .(month_ret = prod(Dretwd + 1) - 1), keyby = .(stock.id, year, month)]

# mkt_ret
mkt <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
mkt <- mkt[, mkt := mkt_rf + rf
	][, .(year, month, mkt, mkt_rf, rf)]

#data <- mkt[stock.month, on = .(year, month)
	#][, ret_rf := month_ret - rf
	#][, tag := ifelse(month == 1 | month == 7, 1, 0)
	#][, tag := cumsum(tag), keyby = .(stock.id)
	#][, N := .N, keyby = .(stock.id, tag)
#][N == 6]

# ������ǰ12���°�
data <- mkt[stock.month, on = .(year, month)
	][, ret_rf := month_ret - rf
	][, tag := seq(1:(.N)), keyby = stock.id]

# beta
reg.roll <- list()
for (i in 12:195) {
	reg.roll[[i]] <- data[tag >= i - 11 & tag <= i, {
		I <- lm(ret_rf ~ mkt_rf) %>% coef() %>% as.list()
	},
	keyby = .(stock.id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:195, reg.roll)
roll <- roll[12:195]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
data <- reg.cof[data, on = .(tag, stock.id)
	][, beta := mkt_rf
	][, .(stock.id, year, month, beta)]

beta <- data[month == 6 | month == 12
	][, sem := ifelse(month == 6, 1, 2)]

## mkt r 
mktr <- mkt
mktr <- mktr[, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][, .(sem_mkt = prod(mkt + 1) - 1), keyby = .(year, sem)
	][, sem_mkt.1 := shift(sem_mkt, n = 1, fill = NA, type = "lead")]

## rt r
stock.sem <- stock[, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(sem_ret = prod(Dretwd + 1) - 1), keyby = .(stock.id, year, sem)
	][, sem_ret.1 := shift(sem_ret, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)]

## proportion
# Wm
# ��ͨ��ֵ
marketvalue <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//�¸�����ͨ��ֵ//TRD_Mnth.xlsx")
marketvalue <- as.data.table(marketvalue)
setnames(marketvalue, "Stkcd", "stock.id")
marketvalue <- marketvalue[month == 12 | month == 6
	][, sem := ifelse(month == 12, 2, 1)
	][, MarketValue := Msmvosd * 1000
	][, .(stock.id, year, sem, MarketValue)]

# ѡ��A��
A <- read_excel("C:/Users/shenfan/Desktop/wechat/Buffett alpha/A��.xlsx")
A <- as.data.table(A)
setnames(A, 1:2, c("code", "name"))
A <- A[, stock.id := substring(code, 1, 6)
	][, .(stock.id, name)]
marketvalue <- A[marketvalue, on = .(stock.id), nomatch = 0]

# match
marketvalue <- marketvalue[, proportion.m := MarketValue / sum(MarketValue), keyby = .(year, sem)
	][, .(stock.id, year, sem, proportion.m)]

# Wi-fund
load("portfolio.RData")
portfolio <- portfolio[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, year, sem, stock.id, MarketValue)]
# ����Ҫɾѡһ��ʲô���͵Ļ���
category <- read_excel("C:/Users/shenfan/Desktop/csr/����data/��������.xlsx")
category <- as.data.table(category)
setnames(category, 1:4, c("code", "name", "category1", "category2"))
category <- category[category2 == "ƫ�ɻ���ͻ���" | category2 == "��ͨ��Ʊ�ͻ���"
	][, id := substring(code, 1, 6)
	][, c("code", "name") := NULL]
portfolio <- category[portfolio, on = .(id), nomatch = 0]


############## ability
# beta, marketvalue, mktr, portfolio
# ability match
ability <- portfolio
ability <- marketvalue[ability, on = .(stock.id, year, sem), nomatch = 0]
ability <- beta[ability, on = .(stock.id, year, sem), nomatch = 0]
ability <- mktr[ability, on = .(year, sem), nomatch = 0]
ability <- stock.sem[ability, on = .(stock.id, year, sem), nomatch = 0]

# calculate
ability <- ability[!is.na(beta)
	][, proportion.s := MarketValue / sum(MarketValue), keyby = .(id, year, sem)
	][, .(timing = sum((proportion.s - proportion.m) * beta * sem_mkt.1), picking = sum((proportion.s - proportion.m) * (sem_ret.1 - beta * sem_mkt.1), na.rm = TRUE)), keyby = .(id, year, sem)]

# fund gap?
# real
load("fund-NAV.RData")
# �¶ȵ�������
fund.sem <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(sem_return = prod(AdjustedNAVGrowth + 1, na.rm = TRUE) - 1), keyby = .(id, year, sem)]

# holding return
load("portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, year, sem, stock.id, MarketValue)]
# �ͺ�
pf <- pf[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]

# stock.s ������
holdret <- stock.sem[pf, on = .(stock.id, year, sem)
	][, proportion.s := MarketValue / sum(MarketValue), keyby = .(id, year, sem)
	][, .(holdret = sum(proportion.s * sem_ret, na.rm = TRUE)), keyby = .(id, year, sem)]

fund.gap <- holdret[fund.sem, on = .(id, year, sem)
	][, fundgap := sem_return - holdret
	][, .(id, year, sem, fundgap)]

# ability
ability <- fund.gap[ability, on = .(id, year, sem)]


save(ability, file = "fundability.RData")