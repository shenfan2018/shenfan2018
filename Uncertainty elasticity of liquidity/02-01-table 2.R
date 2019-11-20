load('uel.RData')

# 将NA变成0
uel <- uel[, colnames(uel[, 3:6]) := lapply(.SD[, 3:6], function(x) {
	ifelse(is.na(x), 0, x)
})]

# table 2
# mean
table <- uel
table <- table[, c("stkcd", "qualified", "Markettype") := NULL
	][, uel.g := ntile(port_uel, 10), keyby = .(year)
	][!is.na(uel.g)
	][, lapply(.SD[, 2:25], mean, na.rm = TRUE), keyby = .(year, uel.g)
	][, lapply(.SD[, 2:25], mean, na.rm = TRUE), keyby = .(uel.g)]

write.csv(table, "C://Users//shenfan//Desktop//mydatam.csv")

# difference
table <- uel
table <- table[, c("stkcd", "qualified", "Markettype") := NULL
	][, uel.g := ntile(port_uel, 10), keyby = .(year)
	][!is.na(uel.g)
	][, lapply(.SD[, 2:25], mean, na.rm = TRUE), keyby = .(year, uel.g)]

table = melt(table, id.vars = 1:2, measure.vars = 3:26)

table <- table[, value.2 := shift(value, n = 9, fill = NA, type = "lead")
	][uel.g == 1
	][, dif := value.2 - value
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(variable)]

write.csv(table, "C://Users//shenfan//Desktop//mydatam.csv")



######################################################################
# 最后用的是这个数据
# date 7.8
load('uel2.RData')

uel <- uel[!is.na(IO)]

# table 2
# mean
table <- uel
table <- table[, c("stkcd", "qualified", "Markettype") := NULL
	][, uel.g := ntile(port_uel, 10), keyby = .(year)
	][!is.na(uel.g)
	][, lapply(.SD[, 2:36], mean, na.rm = TRUE), keyby = .(year, uel.g)
	][, lapply(.SD[, 2:36], mean, na.rm = TRUE), keyby = .(uel.g)]

write.csv(table, "C://Users//shenfan//Desktop//mydatam.csv")

# difference
load('uel2.RData')
uel <- uel[!is.na(IO)]
table <- uel
table <- table[, c("stkcd", "qualified", "Markettype") := NULL
	][, uel.g := ntile(port_uel, 10), keyby = .(year)
	][!is.na(uel.g)
	][, lapply(.SD[, 2:36], mean, na.rm = TRUE), keyby = .(year, uel.g)]

table = melt(table, id.vars = 1:2, measure.vars = 3:37)

table <- table[, value.2 := shift(value, n = 9, fill = NA, type = "lead")
	][uel.g == 1
	][, dif := value.2 - value
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable)]

write.csv(table, "C://Users//shenfan//Desktop//mydatam.csv")