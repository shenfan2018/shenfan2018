load("datamain5.RData")

data<-data.main5

liner1<-plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner2<-plm(r.capm.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner3<-plm(r.fama.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))

liner4<-plm(r.total.c~netflow.1+netflow.2+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner5<-plm(r.capm.c~netflow.1+netflow.2+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner6<-plm(r.fama.c~netflow.1+netflow.2+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))


liner7<-plm(r.total.c~netflow.1+netflow.2+netflow.3+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner8<-plm(r.capm.c~netflow.1+netflow.2+netflow.3+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner9 <- plm(r.fama.c ~ netflow.1 + netflow.2 + netflow.3 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("DateQ", "code"))

#liner10 <- plm(r.total.c ~ netflow.1 + netflow.2 + netflow.3 + netflow.4+logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("DateQ", "code"))
#liner11 <- plm(r.capm.c ~ netflow.1 + netflow.2 + netflow.3 + netflow.4 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("DateQ", "code"))
#liner12 <- plm(r.fama.c ~ netflow.1 + netflow.2 + netflow.3 + netflow.4 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("DateQ", "code"))

stargazer(liner1, liner2, liner3, liner4, liner5, liner6, liner7, liner8, liner9,type = "html", out = "C://Users//shenfan//Desktop//data//lm//XXX//gai.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")))

#netflowµÄËÄ½×

liner4 <- plm(r.total.c ~ netflow.1 + netflow.2 + netflow.3 + netflow.4 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "pooling", index = c("DateQ", "code"))
liner14 <- data[, felm(r.total.c ~ netflow.1 + netflow.2 + netflow.3 + netflow.4+logfund_size.1 + logfund_age.1 + period_return.1 | code + DateQ)]
stargazer(liner1, liner2, liner3, liner4, type = "html", out = "C://Users//shenfan//Desktop//data//lm//XXX//gai.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes")))
stargazer(liner11, liner12, liner13, liner14, type = "html", out = "C://Users//shenfan//Desktop//data//lm//XXX//gai2.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes")))


