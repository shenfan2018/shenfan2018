QFII <- read_excel("C:/Users/shenfan/Desktop/社保/data/QFII重仓流通股.xlsx")
QFII <- as.data.table(QFII)
setnames(QFII, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

QFII <- QFII[, id := substring(code, 1, 6)
	][, c("id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)]

a <- QFII[id == "000001"
	][order(fund, date)]

a<-QFII[fund=="摩根大通银行"]