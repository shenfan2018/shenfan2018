## 社保涉及每只具体的持仓 （social)
#社保重仓 （仅选择quarter==2和quarter==4，暂时先不这么选，是all sample）, 且仅仅选择了1，5，6 以及2012年以后的4，得到socialpf.RData
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)]

# 选择1,4,5,6
social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "fund.company"))
social.m <- social.m[, fund.company := str_replace_all(fund.company, "管理有限公司", "")
	][!is.na(fund.company), .SD
	][, fund.jc := str_replace_all(fund, "全国社保基金", "")
	][, fund.jc := str_replace_all(fund.jc, "组合", "")
	][, head := substring(fund.jc, 1, 1)
	][head == 1 | head == 5 | head == 6 | head == 4
	][, .(fund, head, fund.company)]

social <- social.m[social, on = .(fund), nomatch = 0]

# 删掉4 2012年以前的
social <- social[!(head == 4 & date < as.Date("2012-01-01"))]

## 剔除小于5的
#social <- social[, N := (.N), keyby = .(fund, date)
	#][N > 4]

save(social, file = "socialpf.RData")

## 社保的汇总 类似于IOS
IOS.m <- social[order(date, stock.id)
	][!is.na(MV.current)
	][, .(holding = sum(MV.current)), keyby = .(date, stock.id)
	][, proportion := holding / sum(holding), keyby = .(date)]

save(IOS.m, file = "IOS.RData")

########################################################
# 还是选择了wind，原因wind有904只基金，portfolio有887
## 基金 (已经只剩下股票型和偏股型了）
# csamr上的 2004.1.1-2019.1.1
#读取个股持仓(这里整体选择是年报和中报
#pf1 <- read_excel("C:/Users/shenfan/Desktop/社保/data/portfolio/股票投资明细表2004.1.1-2009.1.1/Fund_Portfolio_Stock.xlsx")
#pf2 <- read_excel("C:/Users/shenfan/Desktop/社保/data/portfolio/股票投资明细表2009.1.1-2014.1.1/Fund_Portfolio_Stock.xlsx")
#pf3 <- read_excel("C:/Users/shenfan/Desktop/社保/data/portfolio/股票投资明细表2014.1.1-2019.1.1/Fund_Portfolio_Stock.xlsx")
#pf4 <- read_excel("C:/Users/shenfan/Desktop/社保/data/portfolio/股票投资明细表2014.1.1-2019.1.1/Fund_Portfolio_Stock1.xlsx")
#pf5 <- read_excel("C:/Users/shenfan/Desktop/社保/data/portfolio/股票投资明细表2014.1.1-2019.1.1/Fund_Portfolio_Stock2.xlsx")

#pf <- rbindlist(list(pf1, pf2, pf3, pf4, pf5))
#rm(pf1, pf2, pf3, pf4, pf5)
#pf <- as.data.table(pf) #pf为个股持仓原始文件，误删
##将数据变成portfolio
#portfolio <- pf
#portfolio <- as.data.table(portfolio)
##选择中报和年报
#portfolio <- portfolio[ReportTypeID == 5 | ReportTypeID == 6, .SD]
##整理，包括去除不要的列和enddate改名
#setnames(portfolio, "EndDate", "date")
#setnames(portfolio, "Symbol", "stock.id")
#setnames(portfolio, 2, "id")
#portfolio <- portfolio[, c("FundID", "ReportTypeID", "Startdate", "InvestmentType") := NULL]
##让proportion等变成数字,这个proportion是占净值比例
#portfolio <- portfolio[, Proportion := as.numeric(as.character(Proportion))
	#][, Rank := as.numeric(as.character(Rank))
	#][, MarketValue := as.numeric(as.character(MarketValue))
	#][, Shares := as.numeric(as.character(Shares))
	#][, date := as.Date(as.character(date))]

##匹配category
#category <- read_excel("C://Users//shenfan//Desktop//基金经理//data//股票型混合型//windstyle.xlsx")
#category <- as.data.table(category)
#setnames(category, 1:4, c("code", "id", "name", "fund.category"))
#category <- category[fund.category == "普通股票型基金" | fund.category == "偏股混合型基金", .SD
	#][, c("code", "id", "fund.category", "name")]

#portfolio <- category[portfolio, on = .(id), nomatch = 0]

#portfolio <- portfolio[order(id, date, Rank)
	#][, total.MV := sum(MarketValue), keyby = .(id, date)
#][, proportion.stock := MarketValue / total.MV]

#save(portfolio, file = "portfolio.RData")

# wind portfolio
fundpf <- read_excel("C:/Users/shenfan/Desktop/社保/data/公募基金持仓变化.xlsx")
fundpf <- as.data.table(fundpf)
setnames(fundpf, 1:7, c("code", "name", "report", "stock.id", "stock.name", "current_hold", "hold_change"))
setnames(fundpf, 9, c("current_mv"))
setnames(fundpf, 12, c("fund_category"))
fundpf <- fundpf[, id := substring(code, 1, 6)
	][, stock.id := substring(stock.id, 1, 6)
	][, year := substring(report, 1, 4)
	][, report2 := substring(report, 6, 7)
	][, sem := ifelse(report2 == "中报", 1, 2)
	][, .(code, id, fund_category, year, sem, stock.id, current_hold, hold_change, current_mv)]
# date
date.q <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date.q <- as.data.table(date.q)
date.q <- date.q[, date := as.Date(date)
	][, year := as.character(year(date))
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .SD[.N], keyby = .(year, sem)]
# match
fundpf <- date.q[fundpf, on = .(year, sem)
	][, .(code, id, fund_category, date, stock.id, current_hold, hold_change, current_mv)]

save(fundpf, file = "fundpf.RData")


# IOF
IOF.m <- portfolio[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][order(year, sem, stock.id)
	][!is.na(MarketValue)
	][, .(holding = sum(MarketValue)), keyby = .(year, sem, date, stock.id)
	][, proportion := holding / sum(holding), keyby = .(year, sem)]

save(IOF.m, file = "IOF.RData")
