load("datamain5.RData")

data<-data.main5
#对netflow分组
data <- data[, L.net := quantile(netflow.1, 0.3, na.rm = TRUE), by = DateQ
	][, H.net := quantile(netflow.1, 0.7, na.rm = TRUE), by = DateQ
	][, group.net := ifelse(netflow.1 < L.net, 1, ifelse(netflow.1 > H.net, 3, 2))]

data.net<-data
data.net.1<-data.net[group.net==1,.SD]
liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.1, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.2<-data.net[group.net==2,.SD]
liner2 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.2, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.net.3<-data.net[group.net==3,.SD]
liner3 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.net.3, model = "within", effect = "twoways", index = c("id", "DateQ"))

data<-data.main5
#对inflow分组
data<-data[,L.in:=quantile(inflow.1,0.3,na.rm=TRUE),by=DateQ][
  ,H.in:=quantile(inflow.1,0.7,na.rm=TRUE),by=DateQ][
    ,group.in:=ifelse(inflow.1<L.in,1,ifelse(inflow.1>H.in,3,2))]

data.in<-data
data.in.1<-data.in[group.in==1,.SD]
liner4 <- plm(r.total.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.in.1, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.in.2<-data.in[group.in==2,.SD]
liner5 <- plm(r.total.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.in.2, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.in.3<-data.in[group.in==3,.SD]
liner6 <- plm(r.total.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.in.3, model = "within", effect = "twoways", index = c("id", "DateQ"))

data<-data.main5
#对outflow分组
data<-data[,L.out:=quantile(outflow.1,0.3,na.rm=TRUE),by=DateQ][
  ,H.out:=quantile(outflow.1,0.7,na.rm=TRUE),by=DateQ][
    ,group.out:=ifelse(outflow.1<L.out,1,ifelse(outflow.1>H.out,3,2))]
data.out<-data
data.out.1<-data.out[group.out==1,.SD]
liner7 <- plm(r.total.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.out.1, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.out.2<-data.out[group.out==2,.SD]
liner8 <- plm(r.total.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.out.2, model = "within", effect = "twoways", index = c("id", "DateQ"))
data.out.3<-data.out[group.out==3,.SD]
liner9 <- plm(r.total.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.out.3, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, liner4, liner5, liner6, liner7, liner8, liner9, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//3.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")))



#做交叉 分三组的
data <- data.main5
#对netflow分组
data <- data[, L.net := quantile(netflow.1, 0.2, na.rm = TRUE), by = DateQ
	][, H.net := quantile(netflow.1, 0.8, na.rm = TRUE), by = DateQ
	][, group.net := ifelse(netflow.1 < L.net, 1, ifelse(netflow.1 > H.net, 3, 2))
	][, low := ifelse(group.net == 1, 1, 0)
	][, median := ifelse(group.net == 2, 1, 0)
	][, high := ifelse(group.net == 3, 1, 0)]

liner1 <- plm(r.total.c ~ netflow.1 * low + netflow.1 * high + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

data<-data.main5
data <- data[, L.net := quantile(inflow.1, 0.2, na.rm = TRUE), by = DateQ
	][, H.net := quantile(inflow.1, 0.8, na.rm = TRUE), by = DateQ
	][, group.net := ifelse(inflow.1 < L.net, 1, ifelse(inflow.1 > H.net, 3, 2))
	][, low := ifelse(group.net == 1, 1, 0)
	][, median := ifelse(group.net == 2, 1, 0)
	][, high := ifelse(group.net == 3, 1, 0)]

liner2 <- plm(r.total.c ~ inflow.1 * low + inflow.1 * high + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

data <- data.main5
data <- data[, L.net := quantile(outflow.1, 0.2, na.rm = TRUE), by = DateQ
	][, H.net := quantile(outflow.1, 0.8, na.rm = TRUE), by = DateQ
	][, group.net := ifelse(outflow.1 < L.net, 1, ifelse(outflow.1 > H.net, 3, 2))
	][, low := ifelse(group.net == 1, 1, 0)
	][, median := ifelse(group.net == 2, 1, 0)
	][, high := ifelse(group.net == 3, 1, 0)]

liner3 <- plm(r.total.c ~ outflow.1 * low + outflow.1 * high + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//4.doc", add.lines = list(c("fund", "yes", "yes", "yes"), c("time", "yes", "yes", "yes")))


# netflow positive negative inflow outflow big and small
#大于0小于0
data <- data.main5
#交叉
data <- data[, positive := ifelse(netflow.1 > 0, 1, 0)]
liner1 <- plm(r.total.c ~ netflow.1 * positive + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

data <- data[, group.in := ntile(inflow.1, 2)
	][, inflow.s := ifelse(group.in == 1, 1, 0)]
liner2 <- plm(r.total.c ~ inflow.1 * inflow.s + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

data <- data[, group.out := ntile(outflow.1, 2)
	][, outflow.s := ifelse(group.out == 1, 1, 0)]
liner3 <- plm(r.total.c ~ outflow.1 * outflow.s + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//7.doc", add.lines = list(c("fund", "yes", "yes", "yes"), c("time", "yes", "yes", "yes")))



data.po <- data[netflow.1 > 0, .SD]
data.po<-data.po[, group := ntile(netflow.1, 2)
	][, big := ifelse(group == 2, 1, 0)]
data.po.b <- data.po[group == 2, .SD]
summary(liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.po.b, model = "within", effect = "twoways", index = c("id", "DateQ")))
data.po.s <- data.po[group == 1, .SD]
summary(liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.po.b, model = "within", effect = "twoways", index = c("id", "DateQ")))

summary(liner1 <- plm(r.total.c ~ netflow.1  + logfund_size.1 + logfund_age.1 + period_return.1, data.po, model = "within", effect = "twoways", index = c("id", "DateQ")))


summary(liner1 <- plm(r.total.c ~ netflow.1 * big + logfund_size.1 + logfund_age.1 + period_return.1, data.po, model = "within", effect = "twoways", index = c("id", "DateQ")))

data.ne <- data[netflow.1 < 0, .SD]
data.ne <- data.ne[, group := ntile(netflow.1, 2)
	][, big := ifelse(group == 2, 1, 0)]
data.ne.b <- data.ne[group == 2, .SD]
summary(liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.ne.b, model = "within", effect = "twoways", index = c("id", "DateQ")))
data.ne.s <- data.ne[group == 1, .SD]
summary(liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.ne.s, model = "within", effect = "twoways", index = c("id", "DateQ")))

summary(liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.ne.s, model = "within", effect = "individual", index = c("id")))



summary(liner1 <- plm(r.total.c ~ netflow.1 * big + logfund_size.1 + logfund_age.1 + period_return.1, data.ne, model = "within", effect = "twoways", index = c("id", "DateQ")))



stargazer(liner1, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//7.doc", add.lines = list(c("fund", "yes"), c("time", "yes")))


#分组
data.nr <- data[netflow.1 < 0, .SD]
liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.nr, model = "within", effect = "twoways", index = c("id", "DateQ"))

data.po <- data[netflow.1 > 0, .SD]
liner2 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.po, model = "within", effect = "twoways", index = c("id", "DateQ"))

data <- data[, inflow.group := ntile(inflow.1, 2)]
data.nr <- data[inflow.group ==1, .SD]
liner3 <- plm(r.total.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.nr, model = "within", effect = "twoways", index = c("id", "DateQ"))

data.po <- data[inflow.group == 2, .SD]
liner4 <- plm(r.total.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.po, model = "within", effect = "twoways", index = c("id", "DateQ"))

data <- data[, outflow.group := ntile(outflow.1, 2)]
data.nr <- data[outflow.group == 1, .SD]
liner5 <- plm(r.total.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.nr, model = "within", effect = "twoways", index = c("id", "DateQ"))

data.po <- data[outflow.group == 2, .SD]
liner6 <- plm(r.total.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data.po, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, liner4, liner5, liner6, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//6.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes")))



