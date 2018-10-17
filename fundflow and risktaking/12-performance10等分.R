load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")

data<-data.main4

#这个要先删了
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]

#对performance分10等分组
data<-data[order(period_return.1)]
data<-data[,group.net:=ntile(period_return.1,10),by=DateQ]

data.net<-data
data.net.1<-data.net[group.net==1,.SD]
liner1<-data.net.1[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]
data.net.2<-data.net[group.net==2,.SD]
liner2<-data.net.2[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]
data.net.3<-data.net[group.net==3,.SD]
liner3<-data.net.3[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]
data.net.4<-data.net[group.net==4,.SD]
liner4<-data.net.4[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]
data.net.5<-data.net[group.net==5,.SD]
liner5<-data.net.5[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]
data.net.6<-data.net[group.net==6,.SD]
liner6<-data.net.6[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]
data.net.7<-data.net[group.net==7,.SD]
liner7<-data.net.7[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]
data.net.8<-data.net[group.net==8,.SD]
liner8<-data.net.8[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]
data.net.9<-data.net[group.net==9,.SD]
liner9<-data.net.9[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]
data.net.10<-data.net[group.net==10,.SD]
liner10<-data.net.10[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1|id+DateQ)]

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,liner10,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//20.doc")

#net flow 的san回归
load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")

data<-data.main4
#这个要先删了
a1<-quantile(data[,outflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,outflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(outflow.1>a1|outflow.1<a2,1,0)][
  extremum=="0",.SD]
#对netflow分组
data<-data[, L.net:=quantile(period_return.1,0.2,na.rm=TRUE),by=DateQ][
  , H.net:=quantile(period_return.1,0.8,na.rm=TRUE),by=DateQ][
    , group.net:=ifelse(period_return.1<L.net,1,ifelse(period_return.1>H.net,3,2))]

data.net<-data
data.net.1<-data.net[group.net==1,.SD]
liner1<-data.net.1[,felm(r.total.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1|id+DateQ)]
data.net.2<-data.net[group.net==2,.SD]
liner2<-data.net.2[,felm(r.total.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1|id+DateQ)]
data.net.3<-data.net[group.net==3,.SD]
liner3<-data.net.3[,felm(r.total.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1|id+DateQ)]

stargazer(liner1,liner2,liner3,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//3.doc")

