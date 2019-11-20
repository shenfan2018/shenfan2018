load("datamain5.RData")

data <- data.main5

#对performance分10等分组
data <- data[order(period_return.1)]
data <- data[, group.net := ntile(period_return.1, 10), by = DateQ]

data.net <- data
data.net.1 <- data.net[group.net == 1, .SD]
liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.1, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.2 <- data.net[group.net == 2, .SD]
liner2 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.2, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.3 <- data.net[group.net == 3, .SD]
liner3 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.3, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.4 <- data.net[group.net == 4, .SD]
liner4 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.4, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.5 <- data.net[group.net == 5, .SD]
liner5 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.5, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.6 <- data.net[group.net == 6, .SD]
liner6 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.6, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.7 <- data.net[group.net == 7, .SD]
liner7 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.7, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.8 <- data.net[group.net == 8, .SD]
liner8 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.8, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.9 <- data.net[group.net == 9, .SD]
liner9 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.9, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.10 <- data.net[group.net == 10, .SD]
liner10 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.10, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, liner4, liner5, liner6, liner7, liner8, liner9, liner10, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//16.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes","yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes","yes")))