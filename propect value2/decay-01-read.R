# 加入时间
date <- read_excel("C:/Users/shenfan/Desktop/prospect value/date.xlsx")
date <- as.data.table(date)
date <- date[, year := year(date)
	][, month := month(date)]

### TK
TK <- read_excel("C:/Users/shenfan/Desktop/prospect value/decay/timedecayingTK20190409.xlsx",sheet = 'TKrho0.8')
TK <- as.data.table(TK)
# 0变成NA
TK[TK == 0] <- NA

TK <- date[TK, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]

TK = melt(TK, id.vars = c("date"), measure.vars = colnames(TK[, -1]))
setnames(TK, 2:3, c("stock.id", "TKrho0.8"))


# TK0.85
TK85 <- read_excel("C:/Users/shenfan/Desktop/prospect value/decay/timedecayingTK20190409.xlsx", sheet = 'TKrho0.85')
TK85 <- as.data.table(TK85)
# 0变成NA
TK85[TK85 == 0] <- NA

TK85 <- date[TK85, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]

TK85 = melt(TK85, id.vars = c("date"), measure.vars = colnames(TK85[, -1]))
setnames(TK85, 2:3, c("stock.id", "TKrho0.85"))


# TK0.9
TK90 <- read_excel("C:/Users/shenfan/Desktop/prospect value/decay/timedecayingTK20190409.xlsx", sheet = 'TKrho0.9')
TK90 <- as.data.table(TK90)
# 0变成NA
TK90[TK90 == 0] <- NA

TK90 <- date[TK90, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]

TK90 = melt(TK90, id.vars = c("date"), measure.vars = colnames(TK90[, -1]))
setnames(TK90, 2:3, c("stock.id", "TKrho0.9"))

# match
TK <- TK85[TK, on = .(date, stock.id)]
TK <- TK90[TK, on = .(date, stock.id)]

# 名字问题
TK <- TK[, stock.id := as.character(stock.id)
	][, stock.id := str_pad(stock.id, 6, side = "left", pad = "0")]

save(TK, file = "decayPV.RData")