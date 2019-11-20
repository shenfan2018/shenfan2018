#different performance
load("datamain5.RData")

data <- data.main5
liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner2 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + alpha_capm.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner3 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + alpha_fama.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner4 <- plm(r.total.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner5 <- plm(r.total.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + alpha_capm.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner6 <- plm(r.total.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + alpha_fama.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner7 <- plm(r.total.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner8 <- plm(r.total.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + alpha_capm.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner9 <- plm(r.total.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + alpha_fama.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, liner4, liner5, liner6, liner7, liner8, liner9, type = "html", out = "C://Users//shenfan//Desktop//data//lm//ÔÙÒ»°æ±¾°É//12.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")))