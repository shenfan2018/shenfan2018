#描述性统计
# 表1
load("fund9.RData")

data <- fund.9

data <- data[order(year,id)
	][, sample := .N, keyby = year
	][, mean.risk := mean(fund.risk), keyby = year
	][, mean.sk := mean(fund.sk, na.rm = TRUE), keyby = year
	][, mean.logfund.size := mean(logfund_size, na.rm = TRUE), keyby = year
	][, mean.logfund.age := mean(logfund_age), keyby = year
	][, mean.netflow := mean(netflow, na.rm = TRUE), keyby = year
	][, mean.inflow := mean(inflow, na.rm = TRUE), keyby = year
	][, mean.outflow := mean(outflow, na.rm = TRUE), keyby = year
	][, mean.return := mean(sem_return), keyby = year
	][, mean.churn.rate := mean(churn.rate, na.rm = TRUE), keyby = year
	][, mean.turnover.rate := mean(turnover.rate, na.rm = TRUE), keyby = year
	][, mean.simple.winner := mean(simple.winner, na.rm = TRUE), keyby = year
	][, mean.wa.winner := mean(wa.winner, na.rm = TRUE), keyby = year
	][, mean.simple.group := mean(simple.group, na.rm = TRUE), keyby = year
	][, mean.wa.group := mean(wa.group, na.rm = TRUE), keyby = year
	][, mean.top.10 := mean(top.10, na.rm = TRUE), keyby = year
	][, mean.stock.proportion := mean(stock.proportion, na.rm = TRUE), keyby = year
	][, mean.fund.beta := mean(beta_capm, na.rm = TRUE), keyby = year
	][, mean.wa.beta := mean(wa.beta, na.rm = TRUE), keyby = year
	][, yesno := duplicated(year)
	][yesno == FALSE, .SD]

write.csv(data, "C://Users//shenfan//Desktop//mydatam.csv")

# 表2
data <- fund.9
# pearson检验
a <- data[, c("netflow", "inflow", "outflow", "sem_return", "logfund_size", "logfund_age")]

b <- cor(a, use = "complete.obs", method = "pearson")
write.csv(b, "C://Users//shenfan//Desktop//mydatam.csv")

# 表3 T期rank和T期的flow
data <- fund.9

data <- data[, per.tb := ntile(rank, 5), keyby = .(year, sem)]
data <- data[per.tb == 1, flow.tb := ntile(netflow, 5)
	][per.tb == 2, flow.tb := ntile(netflow, 5)
	][per.tb == 3, flow.tb := ntile(netflow, 5)
	][per.tb == 4, flow.tb := ntile(netflow, 5)
	][per.tb == 5, flow.tb := ntile(netflow, 5)]


#1-5从小到大, 所以对于rank的话，即per.tb=1,rank最高
t.test(data[per.tb == 5, "outflow"])

t.test(data[flow.tb == 3, "netflow"])

t.test(data[per.tb == 5 & flow.tb == 5, "outflow"])

t.test(data[per.tb == 5 & flow.tb == 1, "outflow"], data[per.tb == 1 & flow.tb == 1, "outflow"], paired = FALSE)

t.test(data[, "inflow"])

t.test(data[per.tb == 1,  "outflow"], data[per.tb == 5 , "outflow"], paired = FALSE)


# 表4 T-1期rank和T-1期的flow
data <- fund.9
data <- data[order(year, sem, category2, - sem_return.1)
	][, rank.1 := sequence(.N), by = .(year, sem, category2)]

# 这里啊 要把NA的去掉哦(这里还是考虑到了的）
data <- data[, na := ifelse(is.na(sem_return.1), 1, 0)]
# 这里把sem_return.1缺失的都删掉了
data <- data[na == 0, .SD
	][, per.tb := ntile(rank.1, 5), keyby = .(year, sem)]
data <- data[per.tb == 1, flow.tb := ntile(netflow.1, 5)
	][per.tb == 2, flow.tb := ntile(netflow.1, 5)
	][per.tb == 3, flow.tb := ntile(netflow.1, 5)
	][per.tb == 4, flow.tb := ntile(netflow.1, 5)
	][per.tb == 5, flow.tb := ntile(netflow.1, 5)]

data[, .N, keyby = .(per.tb, flow.tb)]

#1-5从小到大, 所以对于rank的话，即per.tb=1,rank最高
t.test(data[per.tb == 5, "sem_return"])

t.test(data[flow.tb == 5, "sem_return"])

t.test(data[per.tb == 3 & flow.tb == 3, "sem_return"])

t.test(data[per.tb == 1 & flow.tb == 5 , "sem_return"], data[per.tb == 5 & flow.tb == 5 , "sem_return"], paired = FALSE)

t.test(data[, "sem_return"])

t.test(data[per.tb == 1, "sem_return"], data[per.tb == 5, "sem_return"], paired = FALSE)



load("fund9.RData")
# 表10 T-1期rank和T-1期的flow 做收益率，这里不能用mean
data <- fund.9
data <- data[order(id, year, sem)
	][, str_c(colnames(data)[4:7], str_c(".d", 1)) := lapply(.SD[, 3:6], shift, n = 1, fill = NA, type = "lead"), keyby = id]

data <- data[order(year, sem, category2, - sem_return.1)
	][, rank.1 := sequence(.N), by = .(year, sem, category2)]

# 这里啊 要把NA的去掉哦
data <- data[, na := ifelse(is.na(sem_return.1), 1, 0)]
# 这里把sem_return.1缺失的都删掉了
data <- data[na == 0, .SD
	][, per.tb := ntile(rank.1, 5), keyby = .(year, sem)]
data <- data[per.tb == 1, flow.tb := ntile(netflow.1, 5)
	][per.tb == 2, flow.tb := ntile(netflow.1, 5)
	][per.tb == 3, flow.tb := ntile(netflow.1, 5)
	][per.tb == 4, flow.tb := ntile(netflow.1, 5)
	][per.tb == 5, flow.tb := ntile(netflow.1, 5)]

data[, .N, keyby = .(per.tb, flow.tb)]


#1-5从小到大, 所以对于rank的话，即per.tb=1,rank最高
t.test(data[per.tb == 5, "alpha_4.d1"])

t.test(data[flow.tb == 5, "alpha_4.d1"])

t.test(data[per.tb == 5 & flow.tb == 5, "alpha_4.d1"])

t.test(data[per.tb == 1 & flow.tb == 5, "alpha_4.d1"], data[per.tb == 5 & flow.tb == 5,"alpha_4.d1"], paired = FALSE)

t.test(data[, "alpha_4.d1"])

t.test(data[per.tb == 1, "alpha_4.d1"], data[per.tb == 5, "alpha_4.d1"], paired = FALSE)

t.test(data[flow.tb == 5, "alpha_4.d1"], data[flow.tb == 1, "alpha_4.d1"], paired = FALSE)



# 表turnover和churnrate
data <- fund.9
data <- data[order(year, sem, category2, - sem_return.1)
	][, rank.1 := sequence(.N), by = .(year, sem, category2)]

# 这里啊 要把NA的去掉哦(这里还是考虑到了的）
data <- data[, na := ifelse(is.na(sem_return.1), 1, 0)]
# 这里把sem_return.1缺失的都删掉了
data <- data[na == 0, .SD
	][, per.tb := ntile(rank.1, 5), keyby = .(year, sem)]
data <- data[per.tb == 1, flow.tb := ntile(netflow.1, 5)
	][per.tb == 2, flow.tb := ntile(netflow.1, 5)
	][per.tb == 3, flow.tb := ntile(netflow.1, 5)
	][per.tb == 4, flow.tb := ntile(netflow.1, 5)
	][per.tb == 5, flow.tb := ntile(netflow.1, 5)]

data[, .N, keyby = .(per.tb, flow.tb)]

#1-5从小到大, 所以对于rank的话，即per.tb=1,rank最高
t.test(data[per.tb == 5, "churn.rate.c"])

t.test(data[flow.tb == 5, "churn.rate.c"])

t.test(data[per.tb == 3 & flow.tb == 5, "churn.rate.c"])

t.test(data[per.tb == 3 & flow.tb == 5, "turnover.rate.c"], data[per.tb == 3 & flow.tb == 1, "turnover.rate.c"], paired = FALSE)

t.test(data[, "churn.rate.c"])

t.test(data[per.tb == 1, "churn.rate.c"], data[per.tb == 5, "churn.rate.c"], paired = FALSE)