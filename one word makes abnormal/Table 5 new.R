load("sample.RData")
data <- stock.p[, .(stock.id, hy, num.name, year, month, ret)]

# match year
data <- data[, year.match := ifelse(month > 4, year - 1, year - 2)]

# AQ (没有按照fama macbeth数据结构)
AQ <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/yubin/AQ.csv")
AQ <- AQ[, Stkcd := as.character(Stkcd)
	][, stock.id := str_pad(Stkcd, 6, side = "left", pad = "0")
	][, year.match := year
	][, .(stock.id, year.match, AQ)]
data <- AQ[data, on = .(stock.id, year.match)]

# ANA 孙老师
ANA <- fread('C:/Users/shenfan/Desktop/one word makes abnormal profit/yubin/with_liq_betas.csv')
ANA <- ANA[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year.match := year - 1
	][, .(stock.id, year.match, analyst_coverage)]
data <- ANA[data, on = .(stock.id, year.match)]

# social average
# add social
IOS <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/社保基金持股比例.xlsx')
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IO.social", "date"))
IOS <- IOS[!is.na(IO.social)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, year := year(date)]
# avg
IOS.avg <- IOS[, .(social.avg = mean(IO.social)), keyby = .(stock.id, year)]
setnames(IOS.avg, 'year', 'year.match')
data <- IOS.avg[data, on = .(stock.id, year.match)]

# bull and bear # match 牛为1，熊为0
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
four <- four[, .(year, month, mkt_rf)
	][, bull := ifelse(mkt_rf > 0, 1, 0)]
data <- four[data, on = .(year, month)]

save(data,file = 'table5new.RData')


## 处理
## load
#load('table5new.RData')
#data <- data[, social.avg := ifelse(is.na(social.avg), 0, social.avg)
	#][, .(stock.id, year, month, num.name, ret, bull, social.avg, analyst_coverage, AQ)]

#table = melt(data, id.vars = 1:5, measure.vars = 6:9)

#table <- table[is.na(value)
	#][,]


load('table5new.RData')
data <- data[, social.avg := ifelse(is.na(social.avg), 0, social.avg)]
# ANA
ANA <- data[, .(stock.id, year, month, analyst_coverage, num.name, ret)]
ANA <- ANA[!is.na(analyst_coverage)
	][, ANA.g := ifelse(analyst_coverage == 0, 0, 1), keyby = .(year, month)
	][, .(ret = mean(ret)), keyby = .(year, month, ANA.g, num.name)]

# mean
a <- ANA[, .(ret = mean(ret)), keyby = .(num.name, ANA.g)]

# difference
a <- ANA[, ret2 := shift(ret, n = 1, fill = NA, type = "lead")
	][num.name == 3
	][, dif := ret - ret2
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(ANA.g)]

##### social
social <- data[, .(stock.id, year, month, social.avg, num.name, ret)]
social <- social[, ANA.g := ifelse(social.avg == 0, 0, 1), keyby = .(year, month)
	][, .(ret = mean(ret)), keyby = .(year, month, ANA.g, num.name)]
# mean
a <- social[, .(ret = mean(ret)), keyby = .(num.name, ANA.g)]
# difference
a <- social[, ret2 := shift(ret, n = 1, fill = NA, type = "lead")
	][num.name == 3
	][, dif := ret - ret2
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(ANA.g)]

#### bull
bull <- data[, .(stock.id, year, month, bull, num.name, ret)]
bull <- bull[, .(ret = mean(ret)), keyby = .(year, month,num.name)]
# mean
a <- bull[, .(ret = mean(ret)), keyby = .(num.name, bull)]

#### AQ
AQ <- data[, .(stock.id, year, month, AQ, num.name, ret)]
AQ <- AQ[!is.na(AQ)
	][, AQ.g := ntile(AQ, 5), keyby = .(year, month)
	][, .(ret = mean(ret)), keyby = .(year, month, AQ.g, num.name)]

# mean
a <- AQ[, .(ret = mean(ret)), keyby = .(num.name, ANA.g)]

# difference
a <- AQ[, ret2 := shift(ret, n = 1, fill = NA, type = "lead")
	][num.name == 3
	][, dif := ret - ret2
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(AQ.g)]
