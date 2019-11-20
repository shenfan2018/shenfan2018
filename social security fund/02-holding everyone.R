#社保重仓 仅选择quarter==2和quarter==4
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 2 | quarter == 4
	][, sem := ifelse(quarter == 2, 1, 2)]

# 选择1,5,6
social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "fund.company"))
social.m <- social.m[, fund.company := str_replace_all(fund.company, "管理有限公司", "")
	][!is.na(fund.company), .SD
	][, fund.jc := str_replace_all(fund, "全国社保基金", "")
	][, fund.jc := str_replace_all(fund.jc, "组合", "")
	][, head := substring(fund.jc, 1, 1)
	][head == 1 | head == 5 | head == 6
	][, .(fund, fund.company)]

social <- social.m[social, on = .(fund), nomatch = 0]

### 开始holding total
IOS <- social[order(year, sem, stock.id)
	][!is.na(MV.current)
	][, .(holding = sum(MV.current)), keyby = .(year, sem, date, stock.id)
	][, proportion := holding / sum(holding), keyby = .(year, sem)
	][, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 1, 2, 1)]

load("stock.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, month_ret := prod(Dretwd2) - 1, keyby = .(stock.id, year, month)
	][, vol := sd(Dretwd), keyby = .(stock.id, year, month)
	][, .SD[.N], keyby = .(stock.id, year, month)
	][, .(stock.id, year, quarter, month, month_ret)]



