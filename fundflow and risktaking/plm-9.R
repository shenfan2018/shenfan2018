#net flow 大于0小于0
load("datamain5.RData")

data <- data.main5
#这个要先删了
a1 <- quantile(data[, netflow.1], 0.99, na.rm = TRUE)
a2 <- quantile(data[, netflow.1], 0.01, na.rm = TRUE)
data <- data[, extremum := ifelse(netflow.1 > a1 | netflow.1 < a2, 1, 0)][
    extremum == "0", .SD]
#对netflow分组>0 <0分组
data <- data[, group.net := ifelse(netflow.1 > 0, 1, 0), by = DateQ]

liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("DateQ", "code"))

#1 netflow>0
data.net <- data
data.net.3 <- data.net[group.net == 1, .SD]
liner3 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.3, model = "pooling", index = c("DateQ", "code"))

#0 netflow<0
data.net.2 <- data.net[group.net == 0, .SD]
liner2 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.2, model = "pooling", index = c("DateQ", "code"))

stargazer(liner1, liner3, liner2, type = "html", out = "C://Users//shenfan//Desktop//data//lm//XXX//gai.doc", add.lines = list(c("fund", "yes", "yes", "yes"), c("time", "yes", "yes", "yes")))

data <- data.main4
#这个要先删了
a1 <- quantile(data[, netflow.1], 0.99, na.rm = TRUE)
a2 <- quantile(data[, netflow.1], 0.01, na.rm = TRUE)
data <- data[, extremum := ifelse(netflow.1 > a1 | netflow.1 < a2, 1, 0)][
    extremum == "0", .SD]
#对netflow分组>0 <0分组
data <- data[, group.net := ifelse(netflow.1 > 0, 1, 0), by = DateQ]

#0 netflow<0
data.net.2 <- data.net[group.net == 0, .SD]
#对netflow分组
data.net.2 <- data.net.2[, L.net := quantile(netflow.1, 0.2, na.rm = TRUE), by = DateQ][
    , H.net := quantile(netflow.1, 0.8, na.rm = TRUE), by = DateQ][
	, group.net2 := ifelse(netflow.1 < L.net, 1, ifelse(netflow.1 > H.net, 3, 2))]

data.net.2.1 <- data.net.2[group.net2 == 1, .SD]
liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.2.1, model = "pooling", index = c("DateQ", "code"))
data.net.2.2 <- data.net.2[group.net2 == 2, .SD]
liner2 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.2.2, model = "pooling", index = c("DateQ", "code"))
data.net.2.3 <- data.net.2[group.net2 == 3, .SD]
liner3 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.2.3, model = "pooling", index = c("DateQ", "code"))

#1 netflow>0
data.net.1 <- data.net[group.net == 1, .SD]
#对netflow分组
data.net.1 <- data.net.1[, L.net := quantile(netflow.1, 0.2, na.rm = TRUE), by = DateQ][
    , H.net := quantile(netflow.1, 0.8, na.rm = TRUE), by = DateQ][
	, group.net2 := ifelse(netflow.1 < L.net, 1, ifelse(netflow.1 > H.net, 3, 2))]

data.net.1.1 <- data.net.1[group.net2 == 1, .SD]
liner4 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.1.1, model = "pooling", index = c("DateQ", "code"))
data.net.1.2 <- data.net.1[group.net2 == 2, .SD]
liner5 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.1.2, model = "pooling", index = c("DateQ", "code"))
data.net.1.3 <- data.net.1[group.net2 == 3, .SD]
liner6 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.1.3, model = "pooling", index = c("DateQ", "code"))

stargazer(liner1, liner2, liner3, liner4, liner5, liner6, type = "html", out = "C://Users//shenfan//Desktop//data//lm//XXX//gai.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes")))