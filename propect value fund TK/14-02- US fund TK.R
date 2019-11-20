# add TK
TK1 <- read_excel('C:/Users/shenfan/Desktop/prospect value/data US/TK/USfundTKtemp120190711.xlsx')
TK2 <- read_excel('C:/Users/shenfan/Desktop/prospect value/data US/TK/USfundTKtemp220190711.xlsx')
TK3 <- read_excel('C:/Users/shenfan/Desktop/prospect value/data US/TK/USfundTKtemp320190711.xlsx')
TK4 <- read_excel('C:/Users/shenfan/Desktop/prospect value/data US/TK/USfundTKtemp420190711.xlsx')
TK5 <- read_excel('C:/Users/shenfan/Desktop/prospect value/data US/TK/USfundTKtemp520190711.xlsx')
TK1 <- as.data.table(TK1)
TK2 <- as.data.table(TK2)
TK3 <- as.data.table(TK3)
TK4 <- as.data.table(TK4)
TK5 <- as.data.table(TK5)
# match
TK <- TK1
TK <- TK2[TK, on = .(mcode)]
TK <- TK3[TK, on = .(mcode)]
TK <- TK4[TK, on = .(mcode)]
TK <- TK5[TK, on = .(mcode)]
rm(TK1, TK2, TK3, TK4, TK5)
# ХыАн
fund.TK <- TK
fund.TK = melt(fund.TK, id.vars = 1, measure.vars = 2:22556)
fund.TK <- fund.TK[, fund.id := as.character(variable)
	][, fund.id := str_remove(fund.id, '_')]
setnames(fund.TK, 'value', 'TK')

# match date
date <- read_excel('C:/Users/shenfan/Desktop/prospect value/data US/time.xlsx')
date <- as.data.table(date)
# match
fund.TK <- date[fund.TK, on = .(mcode), nomatch = 0]

fund.TK <- fund.TK[, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)
	][, .(fund.id, year, month, TK)
	][order(fund.id, year, month)]

save(fund.TK, file = 'USTK.RData')
