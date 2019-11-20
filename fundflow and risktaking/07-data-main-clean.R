#进行一些数据的整体改造 包括一些滞后，risk change，performance10等分，age三等分
load("datamain4.RData")

#调整数量级
data.main4 <- data.main4[, netflow := netflow / 100
	][, inflow := inflow / 100
	][, outflow := outflow / 100]

#一些极端值的处理
#对risk.total
data <- data.main4
a1 <- quantile(data[, risk.total], 0.99, na.rm = TRUE)
a2 <- quantile(data[, risk.total], 0.01, na.rm = TRUE)
data <- data[order(risk.total)
	][, extremum := ifelse(risk.total > a1, 2, ifelse(risk.total < a2, 1, 0))
	][extremum == 1, risk.total := risk.total[.N]
	][extremum == 2, risk.total := risk.total[1]]
#1是极小，2是极大

#对risk.adj.total
a1 <- quantile(data[, risk.adj.total], 0.99, na.rm = TRUE)
a2 <- quantile(data[, risk.adj.total], 0.01, na.rm = TRUE)
data <- data[order(risk.adj.total)
	][, extremum := ifelse(risk.adj.total > a1, 2, ifelse(risk.adj.total < a2, 1, 0))
	][extremum == 1, risk.adj.total := risk.adj.total[.N]
	][extremum == 2, risk.adj.total := risk.adj.total[1]]

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
data <- data[order(id, DateQ)
	][, netflow.1 := shift(netflow, n = 1, fill = NA, type = "lag"), keyby = id
	][, netflow.2 := shift(netflow, n = 2, fill = NA, type = "lag"), keyby = id
	][, netflow.3 := shift(netflow, n = 3, fill = NA, type = "lag"), keyby = id
	][, netflow.4 := shift(netflow, n = 4, fill = NA, type = "lag"), keyby = id
	][, inflow.1 := shift(inflow, n = 1, fill = NA, type = "lag"), keyby = id
	][, inflow.2 := shift(inflow, n = 2, fill = NA, type = "lag"), keyby = id
	][, inflow.3 := shift(inflow, n = 3, fill = NA, type = "lag"), keyby = id
	][, outflow.1 := shift(outflow, n = 1, fill = NA, type = "lag"), keyby = id
	][, outflow.2 := shift(outflow, n = 2, fill = NA, type = "lag"), keyby = id
	][, outflow.3 := shift(outflow, n = 3, fill = NA, type = "lag"), keyby = id
	][, period_return.1 := shift(period_return, n = 1, fill = NA, type = "lag"), keyby = id
	][, logfund_age.1 := shift(logfund_age, n = 1, fill = NA, type = "lag"), keyby = id
	][, logfund_size.1 := shift(logfund_size, n = 1, fill = NA, type = "lag"), keyby = id
	][, alpha_fama.1 := shift(alpha_fama, n = 1, fill = NA, type = "lag"), keyby = id
	][, alpha_capm.1 := shift(alpha_capm, n = 1, fill = NA, type = "lag"), keyby = id
	][, zh.bt.c:=c(NA,diff(zh.bt,difference=1)), keyby=id
	][, av.bt.c := c(NA, diff(av.bt, difference = 1)), keyby = id
	][, top.10.stock.c := c(NA, diff(top.10.stock, difference = 1)), keyby = id
	][, top.industry.c := c(NA, diff(top.10.stock, difference = 1)), keyby = id
	][, stock.proportion.c := c(NA, diff(stock.proportion, difference = 1)), keyby = id]

#risk 滞后
data <- data[order(id, DateQ)][
	, r.total.1 := shift(risk.total, n = 1, fill = NA, type = "lag"), keyby = id
	][, r.adj.total.1 := shift(risk.adj.total, n = 1, fill = NA, type = "lag"), keyby = id
	][, risk_capm.1 := shift(risk_capm, n = 1, fill = NA, type = "lag"), keyby = id
	][, risk_fama.1 := shift(risk_fama, n = 1, fill = NA, type = "lag"), keyby = id]

#risk change 
data <- data[, r.total.c := risk.total - r.total.1
	][, r.total.adj.c := risk.adj.total - r.adj.total.1
	][, r.capm.c := risk_capm - risk_capm.1
	][, r.fama.c := risk_fama - risk_fama.1]

#risk change percent
data <- data[, r.total.c.p := (risk.total - r.total.1) / r.total.1
	][, r.total.adj.c.p := (risk.adj.total - r.adj.total.1) / r.adj.total.1
	][, r.capm.c.p := (risk_capm - risk_capm.1) / risk_capm.1
	][, r.fama.c.p := (risk_fama - risk_fama.1) / risk_fama.1]

#加入rank
data<-data[order(DateQ,-period_return)][
	, rank := sequence(.N), by = .(DateQ)]

data.main5 <- data

rm(a1, a2)

save(data.main5,file="datamain5.RData")
