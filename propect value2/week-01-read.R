# 加入时间
date <- read_excel("C:/Users/shenfan/Desktop/prospect value/date.xlsx")
date <- as.data.table(date)
date <- date[, year := year(date)
	][, month := month(date)]

# TK52
TK52 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week/monthlyTKfromweeklyreturn20190409.xlsx", sheet = "TK")
TK52 <- as.data.table(TK52)
TK52 <- TK52[, year := ceiling((mcode - 12) / 12 + 2003)
	][, month := mcode - (year - 2003) * 12]
# 加入时间
TK52 <- date[TK52, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]
TK52 = melt(TK52, id.vars = c("date"), measure.vars = colnames(TK52[, -1]))
setnames(TK52, 2:3, c("stock.id", "TK52"))
TK52 <- TK52[, stock.id := as.character(stock.id)
	][, stock.id := substring(stock.id, 2, 7)
	][, TK52 := as.numeric(TK52)]

week.PV <- TK52

# PW52
PW52 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week/monthlyTKfromweeklyreturn20190409.xlsx", sheet = "PW")
PW52 <- as.data.table(PW52)
PW52 <- PW52[, year := ceiling((mcode - 12) / 12 + 2003)
	][, month := mcode - (year - 2003) * 12]
# 加入时间
PW52 <- date[PW52, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]
PW52 = melt(PW52, id.vars = c("date"), measure.vars = colnames(PW52[, -1]))
setnames(PW52, 2:3, c("stock.id", "PW52"))
PW52 <- PW52[, stock.id := as.character(stock.id)
	][, stock.id := substring(stock.id, 2, 7)
	][, PW52 := as.numeric(PW52)]

week.PV <- PW52[week.PV, on = .(date, stock.id)]

# LA52
LA52 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week/monthlyTKfromweeklyreturn20190409.xlsx", sheet = "LA")
LA52 <- as.data.table(LA52)
LA52 <- LA52[, year := ceiling((mcode - 12) / 12 + 2003)
	][, month := mcode - (year - 2003) * 12]
# 加入时间
LA52 <- date[LA52, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]
LA52 = melt(LA52, id.vars = c("date"), measure.vars = colnames(LA52[, -1]))
setnames(LA52, 2:3, c("stock.id", "LA52"))
LA52 <- LA52[, stock.id := as.character(stock.id)
	][, stock.id := substring(stock.id, 2, 7)
	][, LA52 := as.numeric(LA52)]

week.PV <- LA52[week.PV, on = .(date, stock.id)]

# CC52
CC52 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week/monthlyTKfromweeklyreturn20190409.xlsx", sheet = "CC")
CC52 <- as.data.table(CC52)
CC52 <- CC52[, year := ceiling((mcode - 12) / 12 + 2003)
	][, month := mcode - (year - 2003) * 12]
# 加入时间
CC52 <- date[CC52, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]
CC52 = melt(CC52, id.vars = c("date"), measure.vars = colnames(CC52[, -1]))
setnames(CC52, 2:3, c("stock.id", "CC52"))
CC52 <- CC52[, stock.id := as.character(stock.id)
	][, stock.id := substring(stock.id, 2, 7)
	][, CC52 := as.numeric(CC52)]

week.PV <- CC52[week.PV, on = .(date, stock.id)]


# TK26 
TK26 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week/monthlyTKfromweeklyreturn20190409.xlsx", sheet = "TK26")
TK26 <- as.data.table(TK26)
# 加入时间
TK26 <- date[TK26, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]
TK26 = melt(TK26, id.vars = c("date"), measure.vars = colnames(TK26[, -1]))
setnames(TK26, 2:3, c("stock.id", "TK26"))
TK26 <- TK26[, stock.id := as.character(stock.id)
	][, stock.id := str_pad(stock.id, 6, side = "left", pad = "0")
	][, TK26 := as.numeric(TK26)]

week.PV <- TK26[week.PV, on = .(date, stock.id)]

# PW26
PW26 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week/monthlyTKfromweeklyreturn20190409.xlsx", sheet = "PW26")
PW26 <- as.data.table(PW26)
# 加入时间
PW26 <- date[PW26, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]
PW26 = melt(PW26, id.vars = c("date"), measure.vars = colnames(PW26[, -1]))
setnames(PW26, 2:3, c("stock.id", "PW26"))
PW26 <- PW26[, stock.id := as.character(stock.id)
	][, stock.id := str_pad(stock.id, 6, side = "left", pad = "0")
	][, PW26 := as.numeric(PW26)]

week.PV <- PW26[week.PV, on = .(date, stock.id)]

# LA26
LA26 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week/monthlyTKfromweeklyreturn20190409.xlsx", sheet = "LA26")
LA26 <- as.data.table(LA26)
LA26 <- LA26[, year := ceiling((mcode - 12) / 12 + 2003)
	][, month := mcode - (year - 2003) * 12]
# 加入时间
LA26 <- date[LA26, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]
LA26 = melt(LA26, id.vars = c("date"), measure.vars = colnames(LA26[, -1]))
setnames(LA26, 2:3, c("stock.id", "LA26"))
LA26 <- LA26[, stock.id := as.character(stock.id)
	][, stock.id := substring(stock.id, 2, 7)
	][, LA26 := as.numeric(LA26)]

week.PV <- LA26[week.PV, on = .(date, stock.id)]


# CC26
CC26 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week/monthlyTKfromweeklyreturn20190409.xlsx", sheet = "CC26")
CC26 <- as.data.table(CC26)
CC26 <- CC26[, year := ceiling((mcode - 12) / 12 + 2003)
	][, month := mcode - (year - 2003) * 12]
# 加入时间
CC26 <- date[CC26, on = .(year, month), nomatch = 0
	][, c("year", "month", "mcode") := NULL]
CC26 = melt(CC26, id.vars = c("date"), measure.vars = colnames(CC26[, -1]))
setnames(CC26, 2:3, c("stock.id", "CC26"))
CC26 <- CC26[, stock.id := as.character(stock.id)
	][, stock.id := substring(stock.id, 2, 7)
	][, CC26 := as.numeric(CC26)]

week.PV <- CC26[week.PV, on = .(date, stock.id)]

save(week.PV, file = "week-PV.RData")
