load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")

data<-data.main4
#这个要先删了
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]

liner1<-plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner2<-plm(r.capm.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner3<-plm(r.fama.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))

data<-data.main4
#这个要先删了
a1<-quantile(data[,inflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,inflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(inflow.1>a1|inflow.1<a2,1,0)][
  extremum=="0",.SD]
liner4<-plm(r.total.c~inflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner5<-plm(r.capm.c~inflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner6<-plm(r.fama.c~inflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))

data<-data.main4
#这个要先删了
a1<-quantile(data[,outflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,outflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(outflow.1>a1|outflow.1<a2,1,0)][
  extremum=="0",.SD]
liner7<-plm(r.total.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner8<-plm(r.capm.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))
liner9<-plm(r.fama.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","code"))

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//gai.doc",add.lines =list(c("fund", "yes", "yes","yes","yes","yes","yes","yes","yes","yes"), c("time", "yes","yes","yes","yes","yes","yes","yes","yes","yes")))
