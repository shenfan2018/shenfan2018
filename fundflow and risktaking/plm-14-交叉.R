#performance三等分 做交叉
load("datamain5.RData")

data <- data.main5

#对performance分3组
data.per <- data[order(DateQ, period_return.1)]

data.per <- data.per[, L.net := quantile(period_return.1, 0.2, na.rm = TRUE), by = DateQ][
    , H.net := quantile(period_return.1, 0.8, na.rm = TRUE), by = DateQ][
	, group.net := ifelse(period_return.1 < L.net, 1, ifelse(period_return.1 > H.net, 3, 2))][
	  , period_return.1.L := ifelse(group.net == 1, 1, 0)][
		, period_return.1.M := ifelse(group.net == 2, 1, 0)][
		  , period_return.1.H := ifelse(group.net == 3, 1, 0)]

data.net <- data.per

liner1 <- plm(r.total.c ~ netflow.1 * period_return.1.L + netflow.1 * period_return.1.H + logfund_size.1 + logfund_age.1, data.net, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner2 <- plm(r.total.c ~ inflow.1 * period_return.1.L + inflow.1 * period_return.1.H + logfund_size.1 + logfund_age.1, data.net, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner3 <- plm(r.total.c ~ outflow.1 * period_return.1.L + outflow.1 * period_return.1.H + logfund_size.1 + logfund_age.1, data.net, model = "within", effect = "twoways", index = c("id", "DateQ"))

#对logfund_age分3组
data <- data.main5
data.age <- data[order(DateQ, logfund_age.1)]

data.age <- data.age[, L.net := quantile(logfund_age.1, 0.2, na.rm = TRUE), by = DateQ
	][, H.net := quantile(logfund_age.1, 0.8, na.rm = TRUE), by = DateQ
	][, group.net := ifelse(logfund_age.1 < L.net, 1, ifelse(logfund_age.1 > H.net, 3, 2))
	][, logfund_age.1.L := ifelse(group.net == 1, 1, 0)
	][, logfund_age.1.M := ifelse(group.net == 2, 1, 0)
	][, logfund_age.1.H := ifelse(group.net == 3, 1, 0)]

data.net <- data.age

liner4 <- plm(r.total.c ~ netflow.1 * logfund_age.1.L + netflow.1 * logfund_age.1.H + logfund_size.1 + period_return.1, data.net, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner5 <- plm(r.total.c ~ inflow.1 * logfund_age.1.L + inflow.1 * logfund_age.1.H + logfund_size.1 + period_return.1, data.net, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner6 <- plm(r.total.c ~ outflow.1 * logfund_age.1.L + outflow.1 * logfund_age.1.H + logfund_size.1 + period_return.1, data.net, model = "within", effect = "twoways", index = c("id", "DateQ"))

#对logfund_size
data <- data.main5
data.size <- data[order(DateQ, logfund_size.1)]

data.size <- data.size[, L.net := quantile(logfund_size.1, 0.2, na.rm = TRUE), by = DateQ
	][, H.net := quantile(logfund_size.1, 0.8, na.rm = TRUE), by = DateQ
	][, group.net := ifelse(logfund_size.1 < L.net, 1, ifelse(logfund_size.1 > H.net, 3, 2))
	][, logfund_size.1.L := ifelse(group.net == 1, 1, 0)
	][, logfund_size.1.M := ifelse(group.net == 2, 1, 0)
	][, logfund_size.1.H := ifelse(group.net == 3, 1, 0)]

data.net <- data.size

liner7 <- plm(r.total.c ~ netflow.1 * logfund_size.1.L + netflow.1 * logfund_size.1.H + logfund_age.1 + period_return.1, data.net, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner8 <- plm(r.total.c ~ inflow.1 * logfund_size.1.L + inflow.1 * logfund_size.1.H + logfund_age.1 + period_return.1, data.net, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner9 <- plm(r.total.c ~ outflow.1 * logfund_size.1.L + outflow.1 * logfund_size.1.H + logfund_age.1 + period_return.1, data.net, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, liner4, liner5, liner6, liner7, liner8, liner9, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//10.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")))