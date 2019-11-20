#####�籣
# �������CJ��
#�籣�ز�
social <- read_excel("C:/Users/shenfan/Desktop/�籣/data/�籣����/�籣�����ز���ͨ��.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)]

social.m <- read_excel("C:/Users/shenfan/Desktop/�籣/data/�籣���������.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "fund.company"))
social.m <- social.m[, fund.company := str_replace_all(fund.company, "�������޹�˾", "")
	][!is.na(fund.company), .SD]

social <- social.m[social, on = .(fund)
	][, .(fund, fund.company, date, year, quarter, stock.id, MV.current)]

syf <- social
syf <- syf[order(fund, year, quarter)
	][, proportion := MV.current / sum(MV.current, na.rm = TRUE), keyby = .(fund, year, quarter)]


# �¶�����
load("stock.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, month_ret := prod(Dretwd2) - 1, keyby = .(stock.id, year, month)
	][, .SD[.N], keyby = .(stock.id, year, month)
	][, .(stock.id, year, quarter, month, month_ret)]

# t
holding <- syf[stock.m, on = .(stock.id, year, quarter), nomatch = 0
	]

holding <- holding[, .(ret = sum(proportion * month_ret)), keyby = .(fund, year, month)]

holding <- social.m[holding, on = .(fund)]


# real return
load("fund-NAV.RData")
data.NAV <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := AdjustedNAVGrowth + 1
	][, .(ret.fund = prod(AdjustedNAVGrowth2) - 1), keyby = .(id, year, month)]