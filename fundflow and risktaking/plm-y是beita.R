load("datamain5.RData")
data <- data.main5

liner1 <- plm(zh.bt.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner2 <- plm(av.bt.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner3 <- plm(top.10.stock.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner4 <- plm(stock.proportion.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, liner4, type = "html", out = "C://Users//shenfan//Desktop//data//lm//ÔÙÒ»°æ±¾°É//11.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes")))

summary(plm(top.10.stock.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))