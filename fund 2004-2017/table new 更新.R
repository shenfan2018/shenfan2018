# table 3 开始 performance inflow outflow netflow
data <- fund.9[, .(id, year, sem, rank, inflow, outflow, netflow)]

data <- melt(data, id.vars = c("id", "year", "sem", "rank"), measure.vars = c("inflow", "outflow", "netflow"))

data <- data[, per.g := ntile(rank, 5), keyby = .(year, sem, variable)
	][!is.na(value)
	][, flow.g := ntile(value, 5), keyby = .(year, sem, per.g, variable)]

# mean
a <- data[, lapply(.SD[, 3], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 3], mean, na.rm = TRUE), keyby = .(variable, per.g, flow.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# all
a <- data[, lapply(.SD[, 3], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, flow.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# all all
a <- data[, lapply(.SD[, 3], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 5], mean, na.rm = TRUE), keyby = .(variable)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference flow
a <- data[, lapply(.SD[, 3], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, value.2 := shift(value, n = 4, fill = NA, type = "lag"), keyby = .(variable, year, sem, per.g)
	][, dif := value - value.2
	][flow.g == 5
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, per.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference performance
a <- data[, lapply(.SD[, 3], mean, na.rm = TRUE), keyby = .(variable, year, sem, flow.g, per.g)
	][, value.2 := shift(value, n = 4, fill = NA, type = "lead"), keyby = .(variable, year, sem, flow.g)
	][, dif := value - value.2
	][per.g == 1
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, flow.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")





###############################################################
# other variables
load("fund9.RData")
data <- fund.9

data <- data[!is.na(sem_return.1)
	][order(year, sem, category2, - sem_return.1)
	][, rank.1 := sequence(.N), by = .(year, sem, category2)]

data <- data[, .(id, year, sem, rank.1, netflow.1, fund.risk.c, fund.sk.c, fund.beta.c, wa.beta.c, wa.winner.c, wa.group.c, top.10.c, stock.proportion.c, turnover.rate.c, churn.rate.c)]

data <- melt(data, id.vars = c("id", "year", "sem", "rank.1", "netflow.1"), measure.vars = 6:15)

data <- data[!is.na(value)
	][, per.g := ntile(rank.1, 5), keyby = .(year, sem, variable)
	][!is.na(netflow.1)
	][, flow.g := ntile(netflow.1, 5), keyby = .(year, sem, per.g, variable)]

# mean
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 3], mean, na.rm = TRUE), keyby = .(variable, flow.g, per.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# all
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, flow.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# all all
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 5], mean, na.rm = TRUE), keyby = .(variable)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference flow
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, value.2 := shift(value, n = 4, fill = NA, type = "lag"), keyby = .(variable, year, sem, per.g)
	][, dif := value - value.2
	][flow.g == 5
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, per.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference performance
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, flow.g, per.g)
	][, value.2 := shift(value, n = 4, fill = NA, type = "lead"), keyby = .(variable, year, sem, flow.g)
	][, dif := value - value.2
	][per.g == 1
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, flow.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")


######################################################## final table9
load("fund9.RData")
# 表10 T-1期rank和T-1期的flow 做收益率，这里不能用mean
data <- fund.9
data <- data[order(id, year, sem)
	][, str_c(colnames(data)[4:7], str_c(".d", 1)) := lapply(.SD[, 3:6], shift, n = 1, fill = NA, type = "lead"), keyby = id]

data <- data[order(year, sem, category2, - sem_return.1)
	][, rank.1 := sequence(.N), by = .(year, sem, category2)]

data <- data[, .(id, year, sem, rank.1, netflow.1, alpha_4, alpha_4.d1)]

data <- melt(data, id.vars = c("id", "year", "sem", "rank.1", "netflow.1"), measure.vars = 6:7)

data <- data[!is.na(value)
	][, per.g := ntile(rank.1, 5), keyby = .(year, sem, variable)
	][!is.na(netflow.1)
	][, flow.g := ntile(netflow.1, 5), keyby = .(year, sem, per.g, variable)]

# mean
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 3], mean, na.rm = TRUE), keyby = .(variable, flow.g, per.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# all
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, flow.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# all all
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 5], mean, na.rm = TRUE), keyby = .(variable)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference flow
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, value.2 := shift(value, n = 4, fill = NA, type = "lag"), keyby = .(variable, year, sem, per.g)
	][, dif := value - value.2
	][flow.g == 5
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, per.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference performance
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, flow.g, per.g)
	][, value.2 := shift(value, n = 4, fill = NA, type = "lead"), keyby = .(variable, year, sem, flow.g)
	][, dif := value - value.2
	][per.g == 1
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, flow.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")


######################################################################################################################## other variables new
# other variables
load("fund9.RData")
data <- fund.9

# 这里将下一期的change移到上一期
#data <- data[, lapply(colnames(data[, 43:55]), str_c, ".1") %>% unlist() := lapply(.SD[, 42:54], shift, n = 1, fill = NA, type = "lead"), keyby = .(id)]

data <- data[, colnames(data[, 43:55]) := lapply(.SD[, 42:54], shift, n = 1, fill = NA, type = "lead"), keyby = .(id)]

data <- data[, .(id, year, sem, rank, netflow, fund.risk.c, fund.sk.c, fund.beta.c, wa.beta.c, wa.winner.c, wa.group.c, top.10.c, stock.proportion.c, turnover.rate.c, churn.rate.c)]

data <- melt(data, id.vars = c("id", "year", "sem", "rank", "netflow"), measure.vars = 6:15)

data <- data[!is.na(value)
	][, per.g := ntile(rank, 5), keyby = .(year, sem, variable)
	][!is.na(netflow)
	][, flow.g := ntile(netflow, 5), keyby = .(year, sem, per.g, variable)]

# mean
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 3], mean, na.rm = TRUE), keyby = .(variable, flow.g, per.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# all
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, flow.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# all all
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, lapply(.SD[, 5], mean, na.rm = TRUE), keyby = .(variable)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference flow
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, per.g, flow.g)
	][, value.2 := shift(value, n = 4, fill = NA, type = "lag"), keyby = .(variable, year, sem, per.g)
	][, dif := value - value.2
	][flow.g == 5
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, per.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference performance
a <- data[, lapply(.SD[, 4], mean, na.rm = TRUE), keyby = .(variable, year, sem, flow.g, per.g)
	][, value.2 := shift(value, n = 4, fill = NA, type = "lead"), keyby = .(variable, year, sem, flow.g)
	][, dif := value - value.2
	][per.g == 1
	][, .(dif = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable, flow.g)]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")