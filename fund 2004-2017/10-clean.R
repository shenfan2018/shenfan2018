#进行一些数据的整体改造 包括一些滞后，risk change
load("fund8.RData")

data <- fund.8

#加入rank
data <- data[order(year, sem, category2, - sem_return)
	][, rank := sequence(.N), keyby = .(year, sem, category2)
	][, rank.ratio := rank / (.N), keyby = .(year, sem, category2)]


#一些数量级
data <- data[, top.10 := top.10 / 100
	][, stock.proportion := stock.proportion / 100]

#一些极端值的处理
#对fund.risk
a1 <- quantile(data[, fund.risk], 0.99, na.rm = TRUE)
a2 <- quantile(data[, fund.risk], 0.01, na.rm = TRUE)
data <- data[order(fund.risk)
	][, extremum := ifelse(fund.risk > a1, 2, ifelse(fund.risk < a2, 1, 0))
	][extremum == 1, fund.risk := fund.risk[.N]
	][extremum == 2, fund.risk := fund.risk[1]]
#1是极小，2是极大

#对fund.sk
a1 <- quantile(data[, fund.sk], 0.99, na.rm = TRUE)
a2 <- quantile(data[, fund.sk], 0.01, na.rm = TRUE)
data <- data[order(fund.sk)
	][, extremum := ifelse(fund.sk > a1, 2, ifelse(fund.sk < a2, 1, 0))
	][extremum == 1, fund.sk := fund.sk[.N]
	][extremum == 2, fund.sk := fund.sk[1]]

#对netflow
a1 <- quantile(data[, netflow], 0.99, na.rm = TRUE)
a2 <- quantile(data[, netflow], 0.01, na.rm = TRUE)
data <- data[order(netflow)
	][, extremum := ifelse(netflow > a1, 2, ifelse(netflow < a2, 1, 0))
	][extremum == 1, netflow := netflow[.N]
	][extremum == 2, netflow := netflow[1]]

#对inflow
a1 <- quantile(data[, inflow], 0.99, na.rm = TRUE)
a2 <- quantile(data[, inflow], 0.01, na.rm = TRUE)
data <- data[order(inflow)
	][, extremum := ifelse(inflow > a1, 2, ifelse(inflow < a2, 1, 0))
	][extremum == 1, inflow := inflow[.N]
	][extremum == 2, inflow := inflow[1]]

#对outflow
a1 <- quantile(data[, outflow], 0.99, na.rm = TRUE)
a2 <- quantile(data[, outflow], 0.01, na.rm = TRUE)
data <- data[order(outflow)
	][, extremum := ifelse(outflow > a1, 2, ifelse(outflow < a2, 1, 0))
	][extremum == 1, outflow := outflow[.N]
	][extremum == 2, outflow := outflow[1]]

#对churn.rate
a1 <- quantile(data[, churn.rate], 0.99, na.rm = TRUE)
a2 <- quantile(data[, churn.rate], 0.01, na.rm = TRUE)
data <- data[order(churn.rate)
	][, extremum := ifelse(churn.rate > a1, 2, ifelse(churn.rate < a2, 1, 0))
	][extremum == 1, churn.rate := churn.rate[.N]
	][extremum == 2, churn.rate := churn.rate[1]]

#对turnover.rate
a1 <- quantile(data[, turnover.rate], 0.99, na.rm = TRUE)
a2 <- quantile(data[, turnover.rate], 0.01, na.rm = TRUE)
data <- data[order(turnover.rate)
	][, extremum := ifelse(turnover.rate > a1, 2, ifelse(turnover.rate < a2, 1, 0))
	][extremum == 1, turnover.rate := turnover.rate[.N]
	][extremum == 2, turnover.rate := turnover.rate[1]]

data <- data[order(id, year, sem)]

#fund flow的一些滞后
data <- data[order(id, year, sem)
	][, netflow.1 := shift(netflow, n = 1, fill = NA, type = "lag"), keyby = id
	][, inflow.1 := shift(inflow, n = 1, fill = NA, type = "lag"), keyby = id
	][, outflow.1 := shift(outflow, n = 1, fill = NA, type = "lag"), keyby = id
	][, sem_return.1 := shift(sem_return, n = 1, fill = NA, type = "lag"), keyby = id
	][, logfund_age.1 := shift(logfund_age, n = 1, fill = NA, type = "lag"), keyby = id
	][, logfund_size.1 := shift(logfund_size, n = 1, fill = NA, type = "lag"), keyby = id]

# 一些差分
data <- data[order(id, year, sem)
	][, simple.group.c := c(NA, diff(simple.group, difference = 1)), keyby = id
	][, wa.group.c := c(NA, diff(wa.group, difference = 1)), keyby = id
	][, stock.proportion.c := c(NA, diff(stock.proportion, difference = 1)), keyby = id
	][, top.10.c := c(NA, diff(top.10, difference = 1)), keyby = id
	][, turnover.rate.c := c(NA, diff(turnover.rate, difference = 1)), keyby = id
	][, churn.rate.c := c(NA, diff(churn.rate, difference = 1)), keyby = id
	][, simple.winner.c := c(NA, diff(simple.winner, difference = 1)), keyby = id
	][, wa.winner.c := c(NA, diff(wa.winner, difference = 1)), keyby = id
	][, wa.beta.c := c(NA, diff(wa.beta, difference = 1)), keyby = id
	][, fund.beta.c := c(NA, diff(beta_capm, difference = 1)), keyby = id
	][, fund.risk.c := c(NA, diff(fund.risk, difference = 1)), keyby = id
	][, fund.sk.c := c(NA, diff(fund.sk, difference = 1)), keyby = id
	][, rank.ratio.c := c(NA, diff(rank.ratio, difference = 1)), keyby = id]



fund.9 <- data

save(fund.9, file = "fund9.RData")
