#performance三等分 做交叉
load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")

data<-data.main4

#这个要先删了
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]

#对performance分10等分组
data<-data[order(period_return.1)]

data<-data[, L.net:=quantile(logfund_size.1,0.2,na.rm=TRUE),by=DateQ][
  , H.net:=quantile(logfund_size.1,0.8,na.rm=TRUE),by=DateQ][
    , group.net:=ifelse(logfund_size.1<L.net,1,ifelse(logfund_size.1>H.net,3,2))][
      , logfund_size.1.L:=ifelse(group.net==1,1,0)][
        , logfund_size.1.M:=ifelse(group.net==2,1,0)][
          ,logfund_size.1.H:=ifelse(group.net==3,1,0)]

data.net<-data

liner1<-data.net[,felm(r.total.c~netflow.1*logfund_size.1.L+logfund_size.1+logfund_age.1+period_return.1)]

liner2<-data.net[,felm(r.total.c~netflow.1*logfund_size.1.M+logfund_size.1+logfund_age.1+period_return.1)]

liner3<-data.net[,felm(r.total.c~netflow.1*logfund_size.1.H+logfund_size.1+logfund_age.1+period_return.1)]

liner4<-data.net[,felm(r.total.c~inflow.1*logfund_size.1.L+logfund_size.1+logfund_age.1+period_return.1)]

liner5<-data.net[,felm(r.total.c~inflow.1*logfund_size.1.M+logfund_size.1+logfund_age.1+period_return.1)]

liner6<-data.net[,felm(r.total.c~inflow.1*logfund_size.1.H+logfund_size.1+logfund_age.1+period_return.1)]

liner7<-data.net[,felm(r.total.c~outflow.1*logfund_size.1.L+logfund_size.1+logfund_age.1+period_return.1)]

liner8<-data.net[,felm(r.total.c~outflow.1*logfund_size.1.M+logfund_size.1+logfund_age.1+period_return.1)]

liner9<-data.net[,felm(r.total.c~outflow.1*logfund_size.1.H+logfund_size.1+logfund_age.1+period_return.1)]

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//20.doc")