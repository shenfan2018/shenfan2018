# firm characterstic
time <- c("2004-12-31", "2005-12-31", "2006-12-31", "2007-12-31", "2008-12-31", "2009-12-31", "2010-12-31", "2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31", "2018-12-31")

# cash
cash <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qcash.xlsx")
cash <- as.data.table(cash)
setnames(cash, 1:17, c("code", "name", time))
cash = melt(cash, id.vars = c("code", "name"), measure.vars = time)
setnames(cash, 3:4, c("date", "cash"))
cash <- cash[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, cash)]

variable <- cash

# FCF
FCFF <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qFCFF.xlsx")
FCFF <- as.data.table(FCFF)
setnames(FCFF, 1:17, c("code", "name", time))
FCFF = melt(FCFF, id.vars = c("code", "name"), measure.vars = time)
setnames(FCFF, 3:4, c("date", "FCFF"))
FCFF <- FCFF[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, FCFF)]

variable <- FCFF[variable, on = .(id, date)]

# total asset
asset <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qasset.xlsx")
asset <- as.data.table(asset)
setnames(asset, 1:17, c("code", "name", time))
asset = melt(asset, id.vars = c("code", "name"), measure.vars = time)
setnames(asset, 3:4, c("date", "asset"))
asset <- asset[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, asset)]

variable <- asset[variable, on = .(id, date)]

# CapEx
capex <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qCapEx.xlsx")
capex <- as.data.table(capex)
setnames(capex, 1:17, c("code", "name", time))
capex = melt(capex, id.vars = c("code", "name"), measure.vars = time)
setnames(capex, 3:4, c("date", "CapEx"))
capex <- capex[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, CapEx)]

variable <- capex[variable, on = .(id, date)]

# dividend
dividend <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qdividend.xlsx")
dividend <- as.data.table(dividend)
setnames(dividend, 1:17, c("code", "name", time))
dividend = melt(dividend, id.vars = c("code", "name"), measure.vars = time)
setnames(dividend, 3:4, c("date", "dividend"))
dividend <- dividend[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, dividend)]

variable <- dividend[variable, on = .(id, date)]

# sales
sales <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qsales.xlsx")
sales <- as.data.table(sales)
setnames(sales, 1:17, c("code", "name", time))
sales = melt(sales, id.vars = c("code", "name"), measure.vars = time)
setnames(sales, 3:4, c("date", "sales"))
sales <- sales[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, sales)]

variable <- sales[variable, on = .(id, date)]

# advertising
advertising <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qadvertising.xlsx")
advertising <- as.data.table(advertising)
setnames(advertising, 1:17, c("code", "name", time))
advertising = melt(advertising, id.vars = c("code", "name"), measure.vars = time)
setnames(advertising, 3:4, c("date", "advertising"))
advertising <- advertising[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, advertising)]

variable <- advertising[variable, on = .(id, date)]

# leverage
leverage <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qleverage.xlsx")
leverage <- as.data.table(leverage)
setnames(leverage, 1:17, c("code", "name", time))
leverage = melt(leverage, id.vars = c("code", "name"), measure.vars = time)
setnames(leverage, 3:4, c("date", "leverage"))
leverage <- leverage[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, leverage)]

variable <- leverage[variable, on = .(id, date)]

# ROA
roa <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qroa.xlsx")
roa <- as.data.table(roa)
setnames(roa, 1:17, c("code", "name", time))
roa = melt(roa, id.vars = c("code", "name"), measure.vars = time)
setnames(roa, 3:4, c("date", "ROA"))
roa <- roa[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, ROA)]

variable <- roa[variable, on = .(id, date)]

# ROE
roe <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qroe.xlsx")
roe <- as.data.table(roe)
setnames(roe, 1:17, c("code", "name", time))
roe = melt(roe, id.vars = c("code", "name"), measure.vars = time)
setnames(roe, 3:4, c("date", "ROE"))
roe <- roe[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, ROE)]

variable <- roe[variable, on = .(id, date)]

# R&D
RD <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qR&D.xlsx")
RD <- as.data.table(RD)
setnames(RD, 1:17, c("code", "name", time))
RD = melt(RD, id.vars = c("code", "name"), measure.vars = time)
setnames(RD, 3:4, c("date", "RD"))
RD <- RD[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, RD)]

variable <- RD[variable, on = .(id, date)]

# CFO
CFO <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qCFO.xlsx")
CFO <- as.data.table(CFO)
setnames(CFO, 1:17, c("code", "name", time))
CFO = melt(CFO, id.vars = c("code", "name"), measure.vars = time)
setnames(CFO, 3:4, c("date", "CFO"))
CFO <- CFO[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, CFO)]

variable <- CFO[variable, on = .(id, date)]

# firm efficiency
efficiency <- fread("C:/Users/shenfan/Desktop/csr/firm data/qefficiency.csv")
efficiency <- efficiency[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")
	][, .(id, year, efficiency, manager_ability)]

variable <- variable[, year := year(date)]
variable <- efficiency[variable, on = .(id, year)]

# equity（所有者权益）
equity <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/qequity.xlsx")
equity <- as.data.table(equity)
setnames(equity, 1:17, c("code", "name", time))
equity = melt(equity, id.vars = c("code", "name"), measure.vars = time)
setnames(equity, 3:4, c("date", "equity"))
equity <- equity[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(id, date, equity)]

variable <- equity[variable, on = .(id, date)]

# 流通股市值 12月底
MV <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//月个股流通市值//TRD_Mnth.xlsx")
MV <- as.data.table(MV)
setnames(MV, "Stkcd", "id")
MV <- MV[month == 12
	][, MV := Msmvosd * 1000
	][, .(id, year, MV)]

variable <- MV[variable, on = .(id, year)]

# category
category <- fread("C:/Users/shenfan/Desktop/csr/firm data/qcategory.csv", encoding = "UTF-8", na.strings = "")
category <- category[, id := as.character(Symbol)
	][, id := str_pad(id, 6, side = "left", pad = "0")
	][, date := as.Date(EndDate)
	][, .(id, date, EquityNature, EquityNatureID)]

variable <- category[variable, on = .(id, date)] 

# CSR 2018是report date
csr <- fread("C:/Users/shenfan/Desktop/csr/CSR_09to18.csv")
csr <- csr[, stkcd := as.character(stkcd)
	][, id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year := year - 1
	][, .(id, year, CSR)]

variable <- csr[variable, on = .(id, year)]

# csr 2008-2017
variable <- variable[year > 2007 & year < 2018]

# 一些计算
variable <- variable[, FCF := FCFF / asset
	][, DSRatio := dividend / sales
	][, CFOasset := CFO / asset
	][, ATO := sales / asset
	][, BM := equity / MV]

# 上市日期
IPO <- read_excel("C:/Users/shenfan/Desktop/csr/firm data/上市日期.xlsx")
IPO <- as.data.table(IPO)
setnames(IPO, 1:3, c("code", "name", "IPOdate"))
IPO <- IPO[, id := substring(code, 1, 6)
	][, IPOdate := as.Date(IPOdate)
	][, .(id, IPOdate)]

variable <- IPO[variable, on = .(id)]

# date > IPOdate
variable <- variable[date > IPOdate]

# save 
save(variable, file = "variablesfirm.RData")
