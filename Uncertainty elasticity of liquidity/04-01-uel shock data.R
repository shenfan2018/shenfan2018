# monthly uel
uel.m <- fread('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/uel_month1.csv')
uel.m <- uel.m[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, .(stock.id, year, month, UEL, marketcap)]

# 变成quarter
uel.q <- uel.m[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(uel = mean(UEL)), keyby = .(stock.id, year, quarter)]

# 股灾（取社保的和国家队的）时间，2015二三季度的对比
uel.gz <- uel.q[year == 2015
	][quarter == 2 | quarter == 3]


# social security
# add social
IOS <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/社保基金持股比例.xlsx')
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IO.social", "date"))

IOS <- IOS[!is.na(IO.social)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, year, quarter, IO.social)]

uel.gz <- IOS[uel.gz, on = .(stock.id, year, quarter)]

## add mutual fund
IOF <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/基金持股比例.xlsx')
IOF <- as.data.table(IOF)
setnames(IOF, 1:58, c("code", "name", time))
IOF = melt(IOF, id.vars = c("code", "name"), measure.vars = time)
setnames(IOF, c("value", "variable"), c("IO.fund", "date"))

IOF <- IOF[!is.na(IO.fund)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, year, quarter, IO.fund)]

uel.gz <- IOF[uel.gz, on = .(stock.id, year, quarter)]


# add national 
national <- read_excel("C:/Users/shenfan/Desktop/社保/data/国家队重仓流通股.xlsx")
national <- as.data.table(national)
setnames(national, 1:4, c("code", "stock.name", "national.name", "date"))
setnames(national, 12, "IO.national")

# 2015 二季度到三季度
national <- national[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][year == 2015
	][quarter == 2 | quarter == 3
	][, stock.id := substring(code, 1, 6)
	][, .(IO.national = sum(IO.national)), keyby = .(stock.id, year, quarter)]

uel.gz <- national[uel.gz, on = .(stock.id, year, quarter)]

# write
# uel shock
uel.gz <- uel.gz[, IO.national.0 := ifelse(is.na(IO.national), 0, IO.national)
	][, IO.fund.0 := ifelse(is.na(IO.fund), 0, IO.fund)
	][, IO.social.0 := ifelse(is.na(IO.social), 0, IO.social)]

uel.gz <- uel.gz[, IO.social.buy := ifelse(is.na(IO.social), 0, 1)
	][, IO.fund.buy := ifelse(is.na(IO.fund), 0, 1)
	][, IO.national.buy := ifelse(is.na(IO.national), 0, 1)]

uel.gz <- uel.gz[, IO.social.g := ntile(IO.social.0, 5), keyby = .(quarter)
	][, IO.fund.g := ntile(IO.fund.0, 5), keyby = .(quarter)
	][, IO.national.g := ntile(IO.national.0, 5), keyby = .(quarter)]

save(uel.gz,file = 'uel_shock.RData')

write.csv(uel.gz, "C://Users//shenfan//Desktop//uel_shock.csv")