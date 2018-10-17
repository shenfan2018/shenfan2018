load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")

data<-data.main4
#这个要先删了
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]
#对netflow分组
data<-data[,L.net:=quantile(netflow.1,0.2,na.rm=TRUE),by=DateQ][
  ,H.net:=quantile(netflow.1,0.8,na.rm=TRUE),by=DateQ][
    ,group.net:=ifelse(netflow.1<L.net,1,ifelse(netflow.1>H.net,3,2))]

data.net<-data
data.net.1<-data.net[group.net==1,.SD]
liner1<-plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data.net.1,model="pooling",index=c("DateQ","code"))
data.net.2<-data.net[group.net==2,.SD]
liner2<-plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data.net.2,model="pooling",index=c("DateQ","code"))
data.net.3<-data.net[group.net==3,.SD]
liner3<-plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data.net.3,model="pooling",index=c("DateQ","code"))

data<-data.main4
#这个要先删了inflow
a1<-quantile(data[,inflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,inflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(inflow.1>a1|inflow.1<a2,1,0)][
  extremum=="0",.SD]
#对inflow分组
data<-data[,L.in:=quantile(inflow.1,0.2,na.rm=TRUE),by=DateQ][
  ,H.in:=quantile(inflow.1,0.8,na.rm=TRUE),by=DateQ][
    ,group.in:=ifelse(inflow.1<L.in,1,ifelse(inflow.1>H.in,3,2))]

data.in<-data
data.in.1<-data.in[group.in==1,.SD]
liner4<-plm(r.total.c~inflow.1+logfund_size.1+logfund_age.1+period_return.1,data.in.1,model="pooling",index=c("DateQ","code"))
data.in.2<-data.in[group.in==2,.SD]
liner5<-plm(r.total.c~inflow.1+logfund_size.1+logfund_age.1+period_return.1,data.in.2,model="pooling",index=c("DateQ","code"))
data.in.3<-data.in[group.in==3,.SD]
liner6<-plm(r.total.c~inflow.1+logfund_size.1+logfund_age.1+period_return.1,data.in.3,model="pooling",index=c("DateQ","code"))
data<-data.main4

#这个要先删了outflow
a1<-quantile(data[,outflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,outflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(outflow.1>a1|outflow.1<a2,1,0)][
  extremum=="0",.SD]
#对outflow分组
data<-data[,L.out:=quantile(outflow.1,0.2,na.rm=TRUE),by=DateQ][
  ,H.out:=quantile(outflow.1,0.8,na.rm=TRUE),by=DateQ][
    ,group.out:=ifelse(outflow.1<L.out,1,ifelse(outflow.1>H.out,3,2))]
data.out<-data
data.out.1<-data.out[group.out==1,.SD]
liner7<-plm(r.total.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1,data.out.1,model="pooling",index=c("DateQ","code"))
data.out.2<-data.out[group.out==2,.SD]
liner8<-plm(r.total.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1,data.out.2,model="pooling",index=c("DateQ","code"))
data.out.3<-data.out[group.out==3,.SD]
liner9<-plm(r.total.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1,data.out.3,model="pooling",index=c("DateQ","code"))

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//gai.doc",add.lines =list(c("fund", "yes", "yes","yes","yes","yes","yes","yes","yes","yes"), c("time", "yes","yes","yes","yes","yes","yes","yes","yes","yes")))