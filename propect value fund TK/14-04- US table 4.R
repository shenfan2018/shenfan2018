# 整理 monthly data
# 1991年开始
# month return
fund <- fread('C:/Users/shenfan/Desktop/prospect value/data US/chara.csv')
fund <- fund[, FIRST_OFFER_DT := as.Date(as.character(FIRST_OFFER_DT), format = '%Y%m%d')
	][, CALDT := as.Date(as.character(CALDT), format = '%Y%m%d')
	][, fund_age := as.numeric(difftime(CALDT, FIRST_OFFER_DT, units = c("days"))) / 365
	][, logfund_age := log(fund_age)
	#][, year := as.numeric(substring(CALDT, 1, 4))
	#][, month := as.numeric(substring(CALDT, 5, 6))
	][, logfund_size := log(mtna_n)
	][, month_return := mret_n
	][CALDT > as.Date('1991-01-01')
	][, CRSP_FUNDNO := as.character(CRSP_FUNDNO)
	][, .(CRSP_FUNDNO, CALDT, month_return, logfund_size, logfund_age, flow)
	][, year := year(CALDT)
	][, month := month(CALDT)
	][, quarter := quarter(CALDT)]

# add TK
load('USTK.RData')
setnames(fund.TK, 'fund.id', 'CRSP_FUNDNO')
# match
data <- fund.TK[fund, on = .(CRSP_FUNDNO, year, month)]

# mg6
other <- fread('C:/Users/shenfan/Desktop/prospect value/data US/mg6.csv')
other <- other[, quarter := ifelse(month == 3, 1, ifelse(month == 6, 2, ifelse(month == 9, 3, 4)))
	][, CRSP_FUNDNO := as.character(crsp_fundno)
	][, .(CRSP_FUNDNO, year, quarter, q_timing, q_picking, skill, avg_crkt)]
# match
data <- other[data, on = .(CRSP_FUNDNO, year, quarter)]

# month_return.1
data <- data[, month_return.1 := shift(month_return, n = 1, fill = NA, type = 'lead'), keyby = .(CRSP_FUNDNO)]
# 去重
data <- data[, cf := duplicated(CALDT), keyby = .(CRSP_FUNDNO)
	][cf == FALSE, .SD
	][, cf := NULL]

# add alpha
load('USfundfactor.RData')
# 要运行14-02
alpha <- alpha[, year := as.numeric(substring(Date, 1, 4))
	][, month := as.numeric(substring(Date, 5, 6))
	][, CRSP_FUNDNO := as.character(id)
	][, c("id", "Date", "month_return") := NULL]

data <- alpha[data, on = .(CRSP_FUNDNO, year, month)
	][, lapply(colnames(alpha[, 1:5]), str_c, ".1") %>% unlist() := lapply(.SD[, 1:5], shift, n = 1, fill = NA, type = "lead"), keyby = .(CRSP_FUNDNO)]

data <- data[!is.na(TK)]

write.csv(data, "C://Users//shenfan//Desktop//UStable4monthly.csv")

# TK -1.0502e-01   t TK -11.3551
plm(month_return.1 ~ TK + logfund_size + logfund_age + avg_crkt + flow, data, model = "within", effect = "twoways", index = c("CRSP_FUNDNO", "CALDT")) %>% summary()


