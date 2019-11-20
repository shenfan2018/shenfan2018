# quarterly data
# mg6
other <- fread('C:/Users/shenfan/Desktop/prospect value/data US/mg6.csv')
other <- other[, quarter := ifelse(month == 3, 1, ifelse(month == 6, 2, ifelse(month == 9, 3, 4)))
	][, CRSP_FUNDNO := as.character(crsp_fundno)
	][, .(CRSP_FUNDNO, year, quarter, q_timing, q_picking, skill, avg_crkt)]

# add TK
load('USTK.RData')
setnames(fund.TK, 'fund.id', 'CRSP_FUNDNO')
# match
fund.TK <- fund.TK[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(TK = mean(TK)), keyby = .(CRSP_FUNDNO, year, quarter)]

# logfund_size, logfund_age
fund <- fread('C:/Users/shenfan/Desktop/prospect value/data US/chara.csv')
fund <- fund[, FIRST_OFFER_DT := as.Date(as.character(FIRST_OFFER_DT), format = '%Y%m%d')
	][, CALDT := as.Date(as.character(CALDT), format = '%Y%m%d')
	][, fund_age := as.numeric(difftime(CALDT, FIRST_OFFER_DT, units = c("days"))) / 365
	][, year := year(CALDT)
	][, quarter := quarter(CALDT)
	][, logfund_age := log(fund_age)
	][, logfund_size := log(mtna_n)
	][CALDT > as.Date('1991-01-01')
	][, CRSP_FUNDNO := as.character(CRSP_FUNDNO)
	][, .(logfund_age = mean(logfund_age), logfund_size = mean(logfund_size)), keyby = .(CRSP_FUNDNO, year, quarter)]

# match
data <- fund.TK[fund, on = .(CRSP_FUNDNO, year, quarter)]
data <- other[data, on = .(CRSP_FUNDNO, year, quarter)]

write.csv(data, "C://Users//shenfan//Desktop//UStable8quarterly.csv")
