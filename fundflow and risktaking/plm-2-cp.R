load("datamain5.RData")

data<-data.main5
#这个要先删了
#a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
#a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
#data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
#  extremum=="0",.SD]

liner1 <- plm(r.total.c.p ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner2 <- plm(r.capm.c.p ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner3 <- plm(r.fama.c.p ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

#data<-data.main4
#这个要先删了
#a1<-quantile(data[,inflow.1],0.99,na.rm=TRUE)
#a2<-quantile(data[,inflow.1],0.01,na.rm=TRUE)
#data<-data[,extremum:=ifelse(inflow.1>a1|inflow.1<a2,1,0)][
#  extremum=="0",.SD]
liner4 <- plm(r.total.c.p ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner5 <- plm(r.capm.c.p ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner6 <- plm(r.fama.c.p ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

#data<-data.main4
#这个要先删了
#a1<-quantile(data[,outflow.1],0.99,na.rm=TRUE)
#a2<-quantile(data[,outflow.1],0.01,na.rm=TRUE)
#data<-data[,extremum:=ifelse(outflow.1>a1|outflow.1<a2,1,0)][
#  extremum=="0",.SD]
liner7 <- plm(r.total.c.p ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner8 <- plm(r.capm.c.p ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner9 <- plm(r.fama.c.p ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,type="html",out="C://Users//shenfan//Desktop//data//lm//再一版本吧//2.doc",add.lines =list(c("fund", "yes", "yes","yes","yes","yes","yes","yes","yes","yes"), c("time", "yes","yes","yes","yes","yes","yes","yes","yes","yes")))
