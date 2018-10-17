
load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")


data<-data.main4
#这个要先删了
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]
#对netflow分组>0 <0分组
data<-data[,group.net:=ifelse(netflow.1>0,1,0),by=DateQ]
data.net<-data
#选择1的是>0
data.net.1<-data.net[group.net==1,.SD][
  , L.net:=quantile(netflow.1,0.2,na.rm=TRUE),by=DateQ][
    , H.net:=quantile(netflow.1,0.8,na.rm=TRUE),by=DateQ][
      , group2.net:=ifelse(netflow.1<L.net,1,ifelse(netflow.1>H.net,3,2))]
data.net.1.1<-data.net.1[group2.net==1,.SD]
liner1<-data.net.1.1[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]
data.net.1.2<-data.net.1[group2.net==2,.SD]
liner2<-data.net.1.2[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]
data.net.1.3<-data.net.1[group2.net==3,.SD]
liner3<-data.net.1.3[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]
#选择0的是<0
data.net.0<-data.net[group.net==0,.SD][
  , L.net:=quantile(netflow.1,0.2,na.rm=TRUE),by=DateQ][
    , H.net:=quantile(netflow.1,0.8,na.rm=TRUE),by=DateQ][
      , group2.net:=ifelse(netflow.1<L.net,1,ifelse(netflow.1>H.net,3,2))]

data.net.0.1<-data.net.0[group2.net==1,.SD]
liner4<-data.net.0.1[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]
data.net.0.2<-data.net.0[group2.net==2,.SD]
liner5<-data.net.0.2[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]
data.net.0.3<-data.net.0[group2.net==3,.SD]
liner6<-data.net.0.3[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]

stargazer(liner4,liner5,liner6,liner1,liner2,liner3,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//4.doc")



data<-data.main4
#这个要先删了
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]
#对netflow分组>0 <0分组
data<-data[,group.net:=ifelse(netflow.1>0,1,0),by=DateQ]
data.net<-data
#选择1的是>0
data.net.1<-data.net[group.net==1,.SD][
  , group2.net:=ntile(netflow.1,6)]
data.net.1.1<-data.net.1[group2.net==1,.SD]
liner1<-data.net.1.1[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]
data.net.1.2<-data.net.1[group2.net==2,.SD]
liner2<-data.net.1.2[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]
data.net.1.3<-data.net.1[group2.net==3,.SD]
liner3<-data.net.1.3[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]

#选择0的是<0
data.net.0<-data.net[group.net==0,.SD][
  , group2.net:=ntile(netflow.1,3)]

data.net.0.1<-data.net.0[group2.net==1,.SD]
liner4<-data.net.0.1[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]
data.net.0.2<-data.net.0[group2.net==2,.SD]
liner5<-data.net.0.2[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]
data.net.0.3<-data.net.0[group2.net==3,.SD]
liner6<-data.net.0.3[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|id)]

stargazer(liner4,liner5,liner6,liner1,liner2,liner3,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//3.doc")

