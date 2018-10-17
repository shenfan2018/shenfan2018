#performance三等分 做交叉
load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")

data<-data.main4
#这个要先删了
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]
a1<-quantile(data[,inflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,inflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(inflow.1>a1|inflow.1<a2,1,0)][
  extremum=="0",.SD]
a1<-quantile(data[,outflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,outflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(outflow.1>a1|outflow.1<a2,1,0)][
  extremum=="0",.SD]

#对performance分3组
data.per<-data[order(DateQ,period_return.1)]

data.per<-data.per[, L.net:=quantile(period_return.1,0.2,na.rm=TRUE),by=DateQ][
  , H.net:=quantile(period_return.1,0.8,na.rm=TRUE),by=DateQ][
    , group.net:=ifelse(period_return.1<L.net,1,ifelse(period_return.1>H.net,3,2))][
      , period_return.1.L:=ifelse(group.net==1,1,0)][
        , period_return.1.M:=ifelse(group.net==2,1,0)][
          ,period_return.1.H:=ifelse(group.net==3,1,0)]

data.net<-data.per

liner1<-data.net[,felm(r.total.c~netflow.1*period_return.1.L+netflow.1*period_return.1.M+logfund_size.1+logfund_age.1+period_return.1|DateQ+id)]
liner2<-data.net[,felm(r.total.c~inflow.1*period_return.1.L+inflow.1*period_return.1.M+logfund_size.1+logfund_age.1+period_return.1|DateQ+id)]
liner3<-data.net[,felm(r.total.c~outflow.1*period_return.1.L+outflow.1*period_return.1.M+logfund_size.1+logfund_age.1+period_return.1|DateQ+id)]

#对logfund_age分3组
data.age<-data[order(DateQ,logfund_age.1)]

data.age<-data.age[, L.net:=quantile(logfund_age.1,0.2,na.rm=TRUE),by=DateQ][
  , H.net:=quantile(logfund_age.1,0.8,na.rm=TRUE),by=DateQ][
    , group.net:=ifelse(logfund_age.1<L.net,1,ifelse(logfund_age.1>H.net,3,2))][
      , logfund_age.1.L:=ifelse(group.net==1,1,0)][
        , logfund_age.1.M:=ifelse(group.net==2,1,0)][
          ,logfund_age.1.H:=ifelse(group.net==3,1,0)]

data.net<-data.age

liner4<-data.net[,felm(r.total.c~netflow.1*logfund_age.1.L+netflow.1*logfund_age.1.M+logfund_size.1+logfund_age.1+period_return.1|DateQ+id)]
liner5<-data.net[,felm(r.total.c~inflow.1*logfund_age.1.L+inflow.1*logfund_age.1.M+logfund_size.1+logfund_age.1+period_return.1|DateQ+id)]
liner6<-data.net[,felm(r.total.c~outflow.1*logfund_age.1.L+outflow.1*logfund_age.1.M+logfund_size.1+logfund_age.1+period_return.1|DateQ+id)]

#对logfund_size
data.size<-data[order(DateQ,logfund_size.1)]

data.size<-data.size[, L.net:=quantile(logfund_size.1,0.2,na.rm=TRUE),by=DateQ][
  , H.net:=quantile(logfund_size.1,0.8,na.rm=TRUE),by=DateQ][
    , group.net:=ifelse(logfund_size.1<L.net,1,ifelse(logfund_size.1>H.net,3,2))][
      , logfund_size.1.L:=ifelse(group.net==1,1,0)][
        , logfund_size.1.M:=ifelse(group.net==2,1,0)][
          ,logfund_size.1.H:=ifelse(group.net==3,1,0)]

data.net<-data.size

liner7<-data.net[,felm(r.total.c~netflow.1*logfund_size.1.L+netflow.1*logfund_size.1.M+logfund_size.1+logfund_age.1+period_return.1|DateQ+id)]
liner8<-data.net[,felm(r.total.c~inflow.1*logfund_size.1.L+inflow.1*logfund_size.1.M+logfund_size.1+logfund_age.1+period_return.1|DateQ+id)]
liner9<-data.net[,felm(r.total.c~outflow.1*logfund_size.1.L+outflow.1*logfund_size.1.M+logfund_size.1+logfund_age.1+period_return.1|DateQ+id)]

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//3.doc",add.lines =list(c("fund", "yes", "yes","yes","yes","yes","yes","yes","yes","yes"), c("time", "yes","yes","yes","yes","yes","yes","yes","yes","yes")))