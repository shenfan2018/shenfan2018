load("ability.Rdata")
load("fundfactor.RData")

data <- alpha
data <- data[, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]
data <- ability[data, on = .(id, year, sem)]

# fund Tk group to 1-5
data <- data[, TK.g := ntile(TK, 5), keyby = .(year, month)
	][, .(id, year, month, sem, TK.g, TK, alpha_CAPM, alpha_three, alpha_four, alpha_five, alpha_six, timing, picking, fundgap)
	][!is.na(TK.g)
	][, lapply(.SD[, 3:11], mean, na.rm = TRUE), keyby = .(year, month, TK.g)]
#	][, lapply(.SD[, 3:11], mean, na.rm = TRUE), keyby = .(TK.g)]

#write.csv(data, "C://Users//shenfan//Desktop//11.csv")

data <- melt(data, id.vars = colnames(data[, 1:3]), measures.vars = colnames(data[, 4:12]))
# difference
low <- data[TK.g == 1]
high <- data[TK.g == 5]
dif <- low[high, on = .(year, month, variable)
	][, dif := i.value - value
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(variable)]


write.csv(dif, "C://Users//shenfan//Desktop//11.csv")
