 # 加入到monthly stock but yearly ret
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund/stocklhb.RData")
stock.m <- stock
stock.m <- stock.m[order(stock.id, date)
	][, Dretwd2 := Dretwd + 1
	][, month := month(date)
	][, year := year(date)
	][, .(month_return = prod(Dretwd2) - 1), keyby = .(stock.id, year, month)]

stock.m <- stock.m[, year2 := ifelse(month > 4, year - 1, year - 2)]

## add uel quarterly
#uel.m <- fread('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/uel_month1.csv')
#uel.m <- uel.m[, stkcd := as.character(stkcd)
	#][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	#][, .(stock.id, year, month, UEL)]
## match
#data <- uel.m[stock.m, on = .(stock.id, year, month)]

## add uel quarterly month_return往后移动
#uel.m <- fread('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/uel_month1.csv')
#uel.m <- uel.m[, stkcd := as.character(stkcd)
	#][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	#][, .(stock.id, year, month, UEL)]

## match
#data <- uel.m[stock.m, on = .(stock.id, year, month)
	#][, month_return := shift(month_return, n = 3, fill = NA, type = 'lag'), keyby = .(stock.id)]

# add port_uel
load('uel2.RData')
uel <- uel[, .(stock.id, year, port_uel)]
setnames(uel, 2:3, c('year2', 'UEL'))
# match
data <- uel[stock.m, on = .(stock.id, year2)]

# 这里改哦 (quarter, and year.match) quarter=1, month=2,3,4 
data <- data[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, quarter := shift(quarter, n = 1, fill = NA, type = 'lag'), keyby = .(stock.id)
	][, year.match := ifelse(month == 1, year - 1, year)]

## 这里改哦 (quarter, and year.match) quarter=1, month=1，2，3
#data <- data[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	#][, year.match := year]

## 这里改哦 (quarter, and year.match) quarter=1, month=3,4,5
#data <- data[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	#][, quarter := shift(quarter, n = 2, fill = NA, type = 'lag'), keyby = .(stock.id)
	#][, year.match := ifelse(month == 1 | month == 2, year - 1, year)]

# 到底是不是Buy
load('IOquarter.RData')

IO <- IO[, .(stock.id, date, IOS, IOF, IOI, IOQ, sum)]

## add
#IO <- IO[, lapply(colnames(IO[, 3:7]), str_c, ".shift") %>% unlist() := lapply(.SD[, 2:6], shift, n = 1, fill = NA, type = 'lag'), keyby = .(stock.id)
	#][, IOS.buy := ifelse(IOS > IOS.shift, 1, 0)
	#][, IOF.buy := ifelse(IOF > IOF.shift, 1, 0)
	#][, IOI.buy := ifelse(IOI > IOI.shift, 1, 0)
	#][, IOQ.buy := ifelse(IOQ > IOQ.shift, 1, 0)
	#][, sum.buy := ifelse(sum > sum.shift, 1, 0)]

# hold 状态
IO <- IO[, IOS.buy := ifelse(IOS == 0, 0, 1)
	][, IOF.buy := ifelse(IOF == 0, 0, 1)
	][, IOI.buy := ifelse(IOI == 0, 0, 1)
	][, IOQ.buy := ifelse(IOQ == 0, 0, 1)
	][, sum.buy := ifelse(sum == 0, 0, 1)]

IO <- IO[, year.match := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, year.match, quarter, IOS.buy, IOF.buy, IOI.buy, IOQ.buy, sum.buy)]

data <- IO[data, on = .(stock.id, year.match, quarter)]

# table
data <- data[, .(stock.id, year, month, quarter, month_return, UEL, IOS.buy, IOF.buy, IOI.buy, IOQ.buy, sum.buy)]

table = melt(data, id.vars = 1:6, measure.vars = 7:11)

table <- table[!is.na(UEL)   
	][!is.na(month_return)
	][!is.na(value)
	][, UEL.g := ntile(UEL, 10), keyby = .(variable, year, month, value)]

# mean
a <- table[, .(ret = mean(month_return)), keyby = .(variable, year, month, value, UEL.g)]

# four
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
a <- four[a, on = .(year, month)
	][, rit := ret - rf]

# 总
b <- a[, .(alpha = coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t = summary(lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[, 3][1]), keyby = .(variable, value, UEL.g)]

# dif
dif <- a[, ret.2 := shift(ret, n = 9, fill = NA, type = 'lead')
	][, dif := ret.2 - ret
	][UEL.g == 1
	][, .(alpha = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))[1], t = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[, 3][1], p = summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd))$coef[, 4][1]), keyby = .(variable, value)]

#c <- a[, ret.2 := shift(ret, n = 9, fill = NA, type = 'lead')
	#][, dif := ret.2 - ret
	#][UEL.g == 1]

#summary(lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, c[variable == 'IOS.buy' & value == 0]))$coef[, 4][1]

write.csv(dif, "C://Users//shenfan//Desktop//mydatam.csv")



