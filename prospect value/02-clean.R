load("data.RData")
data <- fund.2

#加入rank
data <- data[order(year, sem, category2, - sem_return)
	][, rank := sequence(.N), keyby = .(year, sem, category2)
	][, rank.ratio := rank / (.N), keyby = .(year, sem, category2)]


#一些极端值的处理
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
	][, zpf.c := c(NA, diff(zpf, difference = 1)), keyby = id
	]

# 删掉2017以及2016第二个半年
data <- data[!year == 2017
	][!year == 2016 | sem == 1]


fund.3 <- data

save(fund.3, file = "data2.RData")
