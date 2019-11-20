load("socialpf.RData")

# semi-annual
social <- social[order(date, fund, MV.current)
	][, .(fund, date, stock.id, name, MV.current)
	][!is.na(MV.current)] 

# Wit
social <- social[, proportion := MV.current / sum(MV.current), keyby = .(date, fund)
	][, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 6, 1, 2)]

# beta
load("stockbeta.RData")
beta <- beta[month == 6 | month == 12
	][, sem := ifelse(month == 6, 1, 2)]

# Rmt
mktr <- fread("C:/Users/shenfan/Desktop/wechat/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
mktr <- as.data.table(mktr)
mktr <- mktr[, mkt := mkt_rf + rf
	][, .(year, month, mkt, rf)]
mktr <- mktr[, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][, mkt2 := mkt + 1
	][, .(sem_mkt = prod(mkt2) - 1), keyby = .(year, sem)
	][, sem_mkt.1 := shift(sem_mkt, n = 1, fill = NA, type = "lag")]

# wm it
MV <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//月个股流通市值//TRD_Mnth.xlsx")
MV <- as.data.table(MV)
setnames(MV, "Stkcd", "stock.id")
MV <- MV[month == 12 | month == 6
	][, sem := ifelse(month == 12, 2, 1)
	][, MV := Msmvosd * 1000
	][, .(stock.id, year, sem, MV)]
# 选择A股
A <- read_excel("C:/Users/shenfan/Desktop/wechat/Buffett alpha/A股.xlsx")
A <- as.data.table(A)
setnames(A, 1:2, c("code", "name"))
A <- A[, stock.id := substring(code, 1, 6)
	][, .(stock.id, name)]
MV <- A[MV, on = .(stock.id), nomatch = 0]
MV <- MV[, proportion.m := MV / sum(MV), keyby = .(year, sem)
	][, .(stock.id, year, sem, proportion.m)]

# ri t+1
load("C:/Users/shenfan/source/repos/shenfan2018/Buffett alpha/stock20030101-20190331.RData")
stock.s <- stock
stock.s <- stock.s[order(stock.id, date)
	][, quarter := quarter(date)
	][, year := year(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, Dretwd2 := Dretwd + 1
	][, .(sem_ret = prod(Dretwd2) - 1), keyby = .(stock.id, year, sem)
	][, sem_ret.1 := shift(sem_ret, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)]


# timing
timing <- social
timing <- MV[timing, on = .(stock.id, year, sem), nomatch = 0]
timing <- beta[timing, on = .(stock.id, year, sem), nomatch = 0]
timing <- mktr[timing, on = .(year, sem), nomatch = 0]

timing <- timing[!is.na(beta)
	][, proportion.s := MV.current / sum(MV.current), keyby = .(fund, year, sem)]

timing <- timing[, .(timing = sum((proportion.s - proportion.m) * beta * sem_mkt.1)), keyby = .(fund, year, sem)]

# picking
picking <- social
picking <- MV[picking, on = .(stock.id, year, sem), nomatch = 0]
picking <- beta[picking, on = .(stock.id, year, sem), nomatch = 0]
picking <- mktr[picking, on = .(year, sem), nomatch = 0]
picking <- stock.s[picking, on = .(stock.id, year, sem), nomatch = 0]

#这里删了
picking <- picking[!is.na(beta)
	][, proportion.s := MV.current / sum(MV.current), keyby = .(fund, year, sem)]

picking <- picking[, .(picking = sum((proportion.s - proportion.m) * (sem_ret.1 - beta * sem_mkt.1), na.rm = TRUE)), keyby = .(fund, year, sem)]

# match
ability <- timing[picking, on = .(fund, year, sem)]

save(ability,file = "ability.RData")
