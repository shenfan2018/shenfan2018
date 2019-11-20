load('sample.RData')
data <- stock.p[, .(stock.id, hy, num.name, year, month, ret)]

# match year
data <- data[, year.match := ifelse(month > 4, year - 1, year - 2)]

## ¼ÓÈë liquid
liq <- fread('C:/Users/shenfan/Desktop/one word makes abnormal profit/yubin/with_liq_betas.csv')
liq <- liq[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year.match := year - 1
	][, .(stock.id, year.match, illiq, port_beta2, port_beta3, port_beta4, port_beta5, size, bm)]

# match
data <- liq[data, on = .(stock.id, year.match)]

# num.name,size,illiq
data <- data[!is.na(illiq)
	][, size.g := ntile(size, 5), keyby = .(year, month, num.name)
	][, .(stock.id, year, month, num.name, hy, size.g, ret, illiq, port_beta2, port_beta3, port_beta4, port_beta5)]

table = melt(data, id.vars = 1:7, measure.vars = 8:12)

table <- table[!is.na(value)
	][, value.g := ntile(value, 5), keyby = .(year, month, variable, num.name, size.g)
	][, .(ret = mean(ret)), keyby = .(year, month, variable, num.name, size.g, value.g)]

a <- table[order(year, month, variable, size.g, value.g, num.name)
	][, ret2 := shift(ret, n = 1, fill = NA, type = 'lead')
	][num.name == 3
	][, dif := ret - ret2
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], dif = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(variable, size.g, value.g)]


