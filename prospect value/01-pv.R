### TK
TK <- fread("C:/Users/shenfan/Desktop/prospect value/data/return_mktTK.csv")
# 0变成NA
TK[TK == 0] <- NA
# 加入时间
date <- read_excel("C:/Users/shenfan/Desktop/prospect value/date.xlsx")
date <- as.data.table(date)
date <- date[, year := year(date)
	][, month := month(date)]
TK <- date[TK, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]

TK = melt(TK, id.vars = c("date"), measure.vars = colnames(TK[, -1]))
setnames(TK, 2:3, c("stock.id", "TK"))

### PW
PW <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/return_mktPW.xlsx")
# 0变成NA
PW[PW == 0] <- NA
PW <- as.data.table(PW)
# 加入时间
PW <- date[PW, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]

PW = melt(PW, id.vars = c("date"), measure.vars = colnames(PW[, -1]))
setnames(PW, 2:3, c("stock.id", "PW"))

### LA
LA <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/return_mktLA.xlsx")
# 0变成NA
LA[LA == 0] <- NA
LA <- as.data.table(LA)
# 加入时间
LA <- date[LA, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]

LA = melt(LA, id.vars = c("date"), measure.vars = colnames(LA[, -1]))
setnames(LA, 2:3, c("stock.id", "LA"))

### CC
CC <- read_excel("C:/Users/shenfan/Desktop/prospect value/data/return_mktCC.xlsx")
# 0变成NA
CC[CC == 0] <- NA
CC <- as.data.table(CC)
# 加入时间
CC <- date[CC, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]

CC = melt(CC, id.vars = c("date"), measure.vars = colnames(CC[, -1]))
setnames(CC, 2:3, c("stock.id", "CC"))

# match
propect <- TK[PW, on = .(date, stock.id)]
propect <- LA[propect, on = .(date, stock.id)]
propect <- CC[propect, on = .(date, stock.id)]

# 名字问题
propect <- propect[, stock.id := as.character(stock.id)
	][, stock.id := str_pad(stock.id, 6, side = "left", pad = "0")]

save(pv, file = "TR.RData")


pv<-pv[order(stock.id, date)
	][, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][!is.na(TK)
	][, .(pv = mean(TK, na.rm = TRUE)), keyby = .(stock.id, year, sem)]


load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

pf <- pv[pf, on = .(stock.id, year, sem), nomatch = 0
	][, proportion.s := MarketValue / sum(MarketValue), keyby = .(id, year, sem)
	][, .(zpf = sum(proportion.s * pv)), keyby = .(id, year, sem)]

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund2.RData")

fund.2 <- pf[fund.2, on = .(id, year, sem)]

save(fund.2, file = "data.RData")

