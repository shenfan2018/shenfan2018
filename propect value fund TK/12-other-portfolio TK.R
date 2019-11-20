# portfolio
load("monthlydata.RData")
load("fundfactor.RData")

fund.TK <- fund.TK[, .(id, year, month, p.TK)]
alpha <- fund.TK[alpha, on = .(id, year, month)]


data <- alpha[, lapply(colnames(alpha[, 6:11]), str_c, ".1") %>% unlist() := lapply(.SD[, 5:10], shift, n = 1L, type = "lead"), keyby = .(id)
	][!is.na(p.TK)
	][!is.na(alpha_CAPM.1)
	][, TK.g := ntile(p.TK, 5), keyby = .(year, month)
	][, .(id, year, month, p.TK, TK.g, month_return.1, alpha_CAPM.1, alpha_three.1, alpha_four.1, alpha_five.1, alpha_six.1)]

data <- melt(data, id.vars = colnames(data[, 1:5]), measures.vars = colnames(data[, 6:11]))

group <- data[, .(ret = mean(value)), keyby = .(year, month, variable, TK.g)
	][!is.na(ret)
	][, .(t = coeftest(lm(ret ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(ret)$estimate), keyby = .(variable, TK.g)]

write.csv(group, "C://Users//shenfan//Desktop//mydatam.csv")

# dif
group <- data[, .(ret = mean(value)), keyby = .(year, month, variable, TK.g)
	][!is.na(ret)
	]

low <- group[TK.g == 1]
high <- group[TK.g == 5]
dif <- low[high, on = .(year, month, variable)
	][, dif := i.ret - ret
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(variable)]

write.csv(dif, "C://Users//shenfan//Desktop//mydatam2.csv")