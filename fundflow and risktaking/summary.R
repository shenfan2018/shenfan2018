#描述性统计
load("datamain5.RData")

data <- data.main5

data <- data[order(DateQ, id)
	][, sample := .N, keyby = year
	][, mean.risk.total := mean(risk.total), keyby = year
	][, mean.fund.size := mean(fund_size, na.rm = TRUE), keyby = year
	][, mean.fund.age := mean(fund_age), keyby = year
	][, mean.netflow := mean(netflow), keyby = year
	][, mean.inflow := mean(inflow), keyby = year
	][, mean.outflow := mean(outflow), keyby = year
	][, mean.return := mean(period_return), keyby=year
	][, mean.zh.bt := mean(zh.bt, na.rm = TRUE), keyby = year
	][, mean.av.bt := mean(av.bt, na.rm = TRUE), keyby = year
	][, mean.top.10.stock := mean(top.10.stock, na.rm = TRUE), keyby = year
	][, mean.top.industry := mean(top.industry, na.rm = TRUE), keyby = year
	][, mean.stock.proportion := mean(stock.proportion, na.rm = TRUE), keyby = year
	][, yesno := duplicated(year)
	][yesno==FALSE, .SD]

write.csv(data, "C://Users//shenfan//Desktop//mydatam.csv")


load("datamain5.RData")
data <- data.main5
data <- data[style == "value", .SD]

data <- data[, per.tb := ntile(period_return.1, 5)]
data <- data[per.tb == 1, flow.tb := ntile(netflow.1, 5)
	][per.tb == 2, flow.tb := ntile(netflow.1, 5)
	][per.tb == 3, flow.tb := ntile(netflow.1, 5)
	][per.tb == 4, flow.tb := ntile(netflow.1, 5)
	][per.tb == 5, flow.tb := ntile(netflow.1, 5)]

#1-5从小到大
t.test(data[per.tb == 5 & flow.tb == 5, "r.total.c"])

t.test(data[, "r.total.c"])

t.test(data[flow.tb == 5 , "r.total.c"])

t.test(data[per.tb == 5, "r.total.c"], data[per.tb == 1, "r.total.c"], paired = FALSE)

t.test(data[flow.tb == 5, "r.total.c"], data[flow.tb == 1, "r.total.c"], paired = FALSE)

t.test(data[per.tb == 5 & flow.tb == 5, "r.total.c"], data[per.tb == 1 & flow.tb == 5, "r.total.c"], paired = FALSE)



t.test(data[tb == 2 , "r.total.c"], data[tb == 1, "r.total.c"], paired = FALSE)



data <- data[, tb := ntile(outflow.1, 2)]
t.test(data[tb == 2, "r.total.c"], data[tb == 1, "r.total.c"], paired = FALSE)
t.test(data[tb == 2, "r.capm.c"], data[tb == 1, "r.capm.c"], paired = FALSE)

#对netflow分组>0 <0分组
data <- data[, group.net := ifelse(netflow.1 > 0, 1, 0)]

data <- data[group.net == 0, tb := ntile(netflow.1, 2)]
t.test(data[group.net == 0 & tb == 1, "r.total.c"], data[group.net == 0 & tb == 2, "r.total.c"], paired = FALSE)


#data <- data[group.net == 0, tb := ntile(netflow.1, 2)]
t.test(data[group.net == 1 & tb == 1, "r.capm.c"], data[group.net == 1 & tb == 2, "r.capm.c"], paired = FALSE)





t.test(data[group.net == 1, "r.total.c"], data[group.net == 0, "r.total.c"], paired = FALSE)
t.test(data[group.net == 1, "r.capm.c"], data[group.net == 0, "r.capm.c"], paired = FALSE)
t.test(data[group.net == 1, "r.fama.c"], data[group.net == 0, "r.fama.c"], paired = FALSE)
t.test(data[group.net == 1, "zh.bt.c"], data[group.net == 0, "zh.bt.c"], paired = FALSE)
t.test(data[group.net == 1, "av.bt.c"], data[group.net == 0, "av.bt.c"], paired = FALSE)
t.test(data[group.net == 1, "top.10.stock"], data[group.net == 0, "top.10.stock"], paired = FALSE)
t.test(data[group.net == 1, "top.industry"], data[group.net == 0, "top.industry"], paired = FALSE)


#再细分
data <- data[, group.net := ifelse(netflow.1 > 0, 1, 0)]

data <- data.main5
data <- data[, mean.r.total.c := mean(r.total.c,na.rm=TRUE), keyby = DateQ
	][, mean.r.capm.c := mean(r.capm.c, na.rm = TRUE), keyby = DateQ
	][, mean.r.fama.c := mean(r.fama.c, na.rm = TRUE), keyby = DateQ
	][, yesno := duplicated(DateQ)
	][yesno == FALSE, .SD]

ggplot(data, aes(DateQ)) +
	geom_line(aes(y = mean.r.total.c, colour = "var0")) +
	geom_line(aes(y = mean.r.capm.c, colour = "var1")) +
	geom_line(aes(y = mean.r.fama.c, colour = "var2"))


a <- data[, c("netflow", "inflow","outflow","period_return", "logfund_size", "logfund_age")]

b <- cor(a, use = "complete.obs", method = "pearson")

write.csv(b, "C://Users//shenfan//Desktop//mydatam.csv")



# 这里可以看看的
hunhe <- data[style == "growth", .SD] 
hunhe <- hunhe[order(DateQ, period_return)
	][, rank := sequence(.N), by = .(DateQ)
	][, rank2 := rank / (.N), by = .(DateQ)
	][order(id, DateQ)
	][, rank.1 := shift(rank, n = 1, fill = NA, type = "lag"), keyby = id
	][, rank2.1 := shift(rank2, n = 1, fill = NA, type = "lag"), keyby = id
	][, rank.c := (rank.1 - rank)
	][, rank.c.a := abs(rank.c)
	][, rank2.c := (rank2.1 - rank2)
	][, rank2.c.a := abs(rank2.c)
	][, rank.c.a.1 := shift(rank.c.a, n = 1, fill = NA, type = "lag"), keyby = id
	][, rank2.c.a.1 := shift(rank2.c.a, n = 1, fill = NA, type = "lag"), keyby = id]

hunhe <- hunhe[, per.tb := ntile(rank.c.a.1, 5),keyby=DateQ]
hunhe <- hunhe[per.tb == 1, flow.tb := ntile(netflow.1, 5)
	][per.tb == 2, flow.tb := ntile(netflow.1, 5)
	][per.tb == 3, flow.tb := ntile(netflow.1, 5)
	][per.tb == 4, flow.tb := ntile(netflow.1, 5)
	][per.tb == 5, flow.tb := ntile(netflow.1, 5)]

#1-5从小到大
t.test(hunhe[per.tb == 5, "r.total.c"])

t.test(hunhe[flow.tb == 5, "r.total.c"])

t.test(hunhe[per.tb == 1 & flow.tb == 5, "r.total.c"])

t.test(hunhe[per.tb == 1 & flow.tb == 5, "r.total.c"], hunhe[per.tb == 1 & flow.tb == 1, "r.total.c"], paired = FALSE)


data <- data.fi
# or

data <- data.main5
## 做rank.1
data <- data[order(DateQ, - period_return)
	][, rank := sequence(.N), by = .(DateQ)
	][order(id, DateQ)
	][, rank.1 := shift(rank, n = 1, fill = NA, type = "lag"), keyby = id
	][, rank.c := (rank.1 - rank)
	][, rank.c.a := abs(rank.c)
	][, rank.c.a.1 := shift(rank.c.a, n = 1, fill = NA, type = "lag"), keyby = id]


data <- data[order(DateQ,-period_return.1)
	][, rank.1 := sequence(.N), by = .(DateQ)]

data <- data[, per.tb := ntile(period_return.1, 5), keyby = DateQ]
data<- data[per.tb == 1, flow.tb := ntile(netflow.1, 5)
	][per.tb == 2, flow.tb := ntile(netflow.1, 5)
	][per.tb == 3, flow.tb := ntile(netflow.1, 5)
	][per.tb == 4, flow.tb := ntile(netflow.1, 5)
	][per.tb == 5, flow.tb := ntile(netflow.1, 5)]

###
mean(data[per.tb == 5 | flow.tb == 1, netflow.1])
mean(data[per.tb == 1 | flow.tb == 1, netflow.1])


#1-5从小到大, 所以对于rank的话，即per.tb=1,rank最高
t.test(data[per.tb == 5, "r.total.c"])

t.test(data[flow.tb == 5, "r.total.c"])

t.test(data[per.tb == 1 & flow.tb == 1, "r.total.c"])

t.test(data[per.tb == 2 & flow.tb == 5, "r.total.c"], data[per.tb == 2& flow.tb == 1, "r.total.c"], paired = FALSE)
