load("datamain6.RData")
load("datamain6.RData")

data <- data.main6
#交叉
#男女
liner1 <- plm(r.total.c ~ netflow.1 * Gender + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner2 <- plm(r.total.c ~ netflow.1 * undergraduate + netflow.1 * PHD + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner3 <- plm(r.total.c ~ netflow.1 * BusinessDuration + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner4 <- plm(r.total.c ~ netflow.1 * Gender + netflow.1 * undergraduate + netflow.1 * PHD + netflow.1 * BusinessDuration + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))


stargazer(liner1, liner2, liner3, liner4, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//13.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes")))


#分组
data.cl <- data[Gender == 1, .SD]
liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.cl, mmodel = "within", effect = "twoways", index = c("id", "DateQ"))
data.cl <- data[Gender == 0, .SD]
liner2 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.cl, model = "within", effect = "twoways", index = c("id", "DateQ"))

#学历
data.cl <- data[master == 1, .SD]
liner3 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.cl, mmodel = "within", effect = "twoways", index = c("id", "DateQ"))
data.cl <- data[PHD == 1, .SD]
liner4 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.cl, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.cl <- data[undergraduate == 1, .SD]
liner5 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.cl, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner5, liner3, liner4, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//14.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes")))

#group
data <- data.main7
liner1 <- plm(r.total.c ~ netflow.1 * group + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

data.cl <- data[group == 0, .SD]
liner2 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.cl, model = "within", effect = "twoways", index = c("id", "DateQ"))

data.cl <- data[group == 1, .SD]
liner3 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.cl, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3,  type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//15.doc", add.lines = list(c("fund", "yes", "yes", "yes"), c("time", "yes", "yes", "yes")))






