# variables
# 分别是csr, vol, number of stocks
# 2009——2018

# csr
csr <- fread("C:/Users/shenfan/Desktop/csr/CSR_09to18.csv")
csr <- csr[, stkcd := as.character(stkcd)
	][, stkcd := str_pad(stkcd, 6, side = "left", pad = "0")
	][, .(stkcd, year, CSR)]

# portfolio
load("portfolio.RData")
setnames(portfolio, "stock.id", "stkcd")
portfolio <- portfolio[, year := year(date)]

# match
variables <- csr[portfolio, on = .(stkcd, year)]

# value weighted
variables <- variables[, sum := sum(MarketValue), keyby = .(id, date)
	][, vw := MarketValue / sum
	][, sum := NULL
	][, .(CSR1 = sum(CSR * vw, na.rm = TRUE), CSR2 = sum(CSR * Proportion / 100, na.rm = TRUE)), keyby = .(id, date, year)
	][year > 2008]

# category
category <- read_excel("C:/Users/shenfan/Desktop/csr/基金data/基金类型.xlsx")
category <- as.data.table(category)
setnames(category, 1:4, c("code", "name", "category1", "category2"))
category <- category[category2 == "偏股混合型基金" | category2 == "普通股票型基金"
	][, id := substring(code, 1, 6)
	][, c("code", "name") := NULL]

variables <- category[variables, on = .(id), nomatch = 0]

# volatility annualized fund return volatility
load("fund-NAV.RData")
NAV <- data.NAV[, year := year(date)
	][year > 2008
	][!is.na(AdjustedNAVGrowth)
	][, .(vol = sd(AdjustedNAVGrowth, na.rm = TRUE)), keyby = .(id, year)]

variables <- NAV[variables, on = .(id, year)]

# number of stocks
load("portfolio.RData")
numstock <- portfolio[, .(numstock = .N), keyby = .(id, date)]

variables <- numstock[variables, on = .(id, date)]

# expense ratio fee
fee <- read_excel("C:/Users/shenfan/Desktop/csr/基金data/费率变动文件/Fund_FeesChange2.xlsx")
fee <- as.data.table(fee)
fee <- fee[NameOfFee == "管理费率"
	][, .(Symbol, DeclareDate, NameOfFee, ProportionOfFee)]
setnames(fee, 1:2, c("id", "date"))

fee <- fee[, date := as.Date(date)
	][, year := year(date)
	][!is.na(ProportionOfFee)
	][, .(fee = mean(as.numeric(ProportionOfFee))), keyby = .(id, year)]

variables <- fee[variables, on = .(id, year)]

# 来源于wind
time <- c("2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31", "2018-06-30", "2018-12-31")

# net flow sem
flow <- read_excel("C:/Users/shenfan/Desktop/csr/基金data/fund flow sem.xlsx")
flow <- as.data.table(flow)
setnames(flow, 1:32, c("code", "name", time))
flow = melt(flow, id.vars = c("code", "name"), measure.vars = time)
setnames(flow, 3:4, c("date", "flow"))
flow <- flow[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, flow)]

## 这里nomatch把portfolio的问题给去掉了
variables <- flow[variables, on = .(id, date), nomatch = 0]


# fund size 基金资产净值
size <- read_excel("C:/Users/shenfan/Desktop/csr/基金data/基金资产净值 sem.xlsx")
size <- as.data.table(size)
setnames(size, 1:32, c("code", "name", time))
size = melt(size, id.vars = c("code", "name"), measure.vars = time)
setnames(size, 3:4, c("date", "size"))
size <- size[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, size)]

variables <- size[variables, on = .(id, date)]

# fund age
age <- read_excel("C:/Users/shenfan/Desktop/csr/基金data/成立日.xlsx")
age <- as.data.table(age)
setnames(age, 1:3, c("code", "name", "startdate"))
age <- age[, id := substring(code, 1, 6)
	][, .(id, startdate)]
variables <- age[variables, on = .(id)
	][, age := as.numeric(difftime(date, startdate, units = c("days")) / 365)]

#############
# turnover
totalbuy <- read_excel("C:/Users/shenfan/Desktop/csr/基金data/买入股票总成本.xlsx")
totalbuy <- as.data.table(totalbuy)
setnames(totalbuy, 1:32, c("code", "name", time))
totalbuy = melt(totalbuy, id.vars = c("code", "name"), measure.vars = time)
setnames(totalbuy, 3:4, c("date", "totalbuy"))

tor <- totalbuy

totalsell <- read_excel("C:/Users/shenfan/Desktop/csr/基金data/卖出股票总收入.xlsx")
totalsell <- as.data.table(totalsell)
setnames(totalsell, 1:32, c("code", "name", time))
totalsell <- melt(totalsell, id.vars = c("code", "name"), measure.vars = time)
setnames(totalsell, 3:4, c("date", "totalsell"))

tor <- totalsell[tor, on = .(code, name, date)]

#报告期股票市值
stockMV <- read_excel("C:/Users/shenfan/Desktop/csr/基金data/股票投资市值.xlsx")
stockMV <- as.data.table(stockMV)
setnames(stockMV, 1:32, c("code", "name", time))
stockMV = melt(stockMV, id.vars = c("code", "name"), measure.vars = time)
setnames(stockMV, 3:4, c("date", "totalMV"))

tor <- stockMV[tor, on = .(code, name, date)]

# minimum of aggregated sales or aggregated purchases of security, divided by 股票市值
tor <- tor[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][totalMV > 0
	][!is.na(totalbuy) & !is.na(totalsell)
	][, turnover := min(totalbuy, totalsell,na.rm = TRUE) / totalMV, keyby = .(id, date)
	][, .(id, date, turnover)]

variables <- tor[variables, on = .(id, date)]

save(variables, file = "variablesfund.RData")

