load("datamain6.RData")

data <- data.main6

summary(liner1 <- plm(r.total.c ~ netflow.1 + av.bt + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))


summary(liner1 <- plm(r.total.c ~ netflow.1 * zh.bt + netflow.1 * stock.proportion + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))

summary(liner1 <- plm(r.total.c ~ netflow.1 * Gender + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))

summary(liner1 <- plm(r.total.c ~ netflow.1 * BusinessDuration + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))

data.group<-data[undergraduate==1,.SD]
summary(liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.group, model = "within", effect = "twoways", index = c("id", "DateQ")))

data <- data[Degree == "硕士研究生"|Degree=="MBA/EMBA", degree := 2
	][Degree == "本科生"|Degree=="大专生", degree := 1
	][Degree == "博士研究生", degree := 3]

summary(liner1 <- plm(r.total.c ~ netflow.1 * degree + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))



data.man <- data[Degree=="硕士研究生", .SD]
summary(liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.man, model = "pooling", index = c("id", "DateQ")))
data.woman <- data[Degree == "博士研究生", .SD]
summary(liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.woman, model = "pooling", index = c("id", "DateQ")))

summary(liner1 <- plm(r.total.c ~ netflow.1 * master + netflow.1 * PHD + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("id", "DateQ")))


summary(liner1 <- plm(r.total.c ~ netflow.1 * group + logfund_size.1 + logfund_age.1 +period_return.1, data, model = "pooling", index = c("id", "DateQ")))

summary(liner1 <- data[, felm(r.total.c ~ netflow.1 * Gender + logfund_size.1 + logfund_age.1 + period_return.1 | code + DateQ)])


summary(plm(r.total.c ~ netflow.1 * growth + netflow.1 * value + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("id", "DateQ")))


load("datamain5.RData")
data <- data.main5
summary(plm(r.total.c ~ netflow.1 * top.10.stock  + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("id", "DateQ")))

summary(plm(r.total.c ~ netflow.1 * zh.bt + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("id", "DateQ")))

#two-ways 是两个都控制
summary(liner1 <- plm(r.total.c ~ netflow.1 * growth + netflow.1 * mixed + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))
summary(liner1 <- plm(r.total.c ~ netflow.1 * growth + netflow.1 * mixed + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))


summary(fixef(liner1, type = "level"))
summary(fixef(liner1, type = "dmean"))
summary(fixef(liner1))[, c("Estimate", "Pr(>|t|)")]

summary(data[, felm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1 | code + DateQ)])



write.csv(data.main5, "C://Users//shenfan//Desktop//shuju.csv")


#readbeta
beta <- read_excel("C://Users//shenfan//Desktop//data//2010-2017股票beta.xlsx")
setnames(beta, 1:34, c("code", "name", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
beta = melt(beta, id.vars = c("code", "name"), measure.vars = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
beta <- as.data.table(beta)
setnames(beta, 3, "date")
beta <- beta[, date := as.Date(as.character(date))
	][, id := (substring(code, 1, 6))
	][name != "NA"] #NA 是删掉wind的注释




load("datacapm.RData")
