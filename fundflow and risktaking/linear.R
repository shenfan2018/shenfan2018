load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")

data<-data.main4
#去除极端值
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]
#回归
liner1<-data[,felm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner2<-data[,felm(r.capm.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner3<-data[,felm(r.fama.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]

data<-data.main4
#去除极端值
a1<-quantile(data[,inflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,inflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(inflow.1>a1|inflow.1<a2,1,0)][
  extremum=="0",.SD]
#回归
liner4<-data[,felm(r.total.c~inflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner5<-data[,felm(r.capm.c~inflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner6<-data[,felm(r.fama.c~inflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]

data<-data.main4
#去除极端值
a1<-quantile(data[,outflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,outflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(outflow.1>a1|outflow.1<a2,1,0)][
  extremum=="0",.SD]
#回归
liner7<-data[,felm(r.total.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner8<-data[,felm(r.capm.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner9<-data[,felm(r.fama.c~outflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//3.doc",add.lines =list(c("fund", "yes", "yes","yes","yes","yes","yes","yes","yes","yes"), c("time", "yes","yes","yes","yes","yes","yes","yes","yes","yes")))


#滞后fund flow2
data<-data.main4
#去除极端值
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]

#回归
liner1<-data[,felm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner2<-data[,felm(r.capm.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner3<-data[,felm(r.fama.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]

data<-data.main4
#去除极端值
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]

a1<-quantile(data[,netflow.2],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.2],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.2>a1|netflow.2<a2,1,0)][
  extremum=="0",.SD]
#回归
liner4<-data[,felm(r.total.c~netflow.1+netflow.2+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner5<-data[,felm(r.capm.c~netflow.1+netflow.2+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner6<-data[,felm(r.fama.c~netflow.1+netflow.2+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]

data<-data.main4
#去除极端值
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]

a1<-quantile(data[,netflow.2],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.2],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.2>a1|netflow.2<a2,1,0)][
  extremum=="0",.SD]

a1<-quantile(data[,netflow.3],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.3],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.3>a1|netflow.3<a2,1,0)][
  extremum=="0",.SD]
#回归
liner7<-data[,felm(r.total.c~netflow.1+netflow.2+netflow.3+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner8<-data[,felm(r.capm.c~netflow.1+netflow.2+netflow.3+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner9<-data[,felm(r.fama.c~netflow.1+netflow.2+netflow.3+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//3.doc",add.lines =list(c("fund", "yes", "yes","yes","yes","yes","yes","yes","yes","yes"), c("time", "yes","yes","yes","yes","yes","yes","yes","yes","yes")))


#滞后fund flow
data<-data.main4
#去除极端值
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]
#回归
liner1<-data[,felm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner2<-data[,felm(r.capm.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner3<-data[,felm(r.fama.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]

data<-data.main4
#去除极端值
a1<-quantile(data[,netflow.2],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.2],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.2>a1|netflow.2<a2,1,0)][
  extremum=="0",.SD]
#回归
liner4<-data[,felm(r.total.c~netflow.2+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner5<-data[,felm(r.capm.c~netflow.2+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner6<-data[,felm(r.fama.c~netflow.2+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]

data<-data.main4
#去除极端值
a1<-quantile(data[,netflow.3],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.3],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.3>a1|netflow.3<a2,1,0)][
  extremum=="0",.SD]
#回归
liner7<-data[,felm(r.total.c~netflow.3+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner8<-data[,felm(r.capm.c~netflow.3+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]
liner9<-data[,felm(r.fama.c~netflow.3+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)]

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//3.doc",add.lines =list(c("fund", "yes", "yes","yes","yes","yes","yes","yes","yes","yes"), c("time", "yes","yes","yes","yes","yes","yes","yes","yes","yes")))



load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")

data<-data.main4
#performance
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]
#回归
liner1<-data[,felm(r.total.c~netflow.1+logfund_size+logfund_age+period_return|code+DateQ)]
liner2<-data[,felm(r.total.c~netflow.1+logfund_size+logfund_age+alpha_capm|code+DateQ)]
liner3<-data[,felm(r.total.c~netflow.1+logfund_size+logfund_age+alpha_fama|code+DateQ)]
liner4<-data[,felm(r.total.c~netflow.1+logfund_size+logfund_age+rank|code+DateQ)]

data<-data.main4
#performance
a1<-quantile(data[,inflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,inflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(inflow.1>a1|inflow.1<a2,1,0)][
  extremum=="0",.SD]
#回归
liner5<-data[,felm(r.total.c~inflow.1+logfund_size+logfund_age+period_return|code+DateQ)]
liner6<-data[,felm(r.total.c~inflow.1+logfund_size+logfund_age+alpha_capm|code+DateQ)]
liner7<-data[,felm(r.total.c~inflow.1+logfund_size+logfund_age+alpha_fama|code+DateQ)]
liner8<-data[,felm(r.total.c~inflow.1+logfund_size+logfund_age+rank|code+DateQ)]

data<-data.main4
#performance
a1<-quantile(data[,outflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,outflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(outflow.1>a1|outflow.1<a2,1,0)][
  extremum=="0",.SD]
#回归
liner9<-data[,felm(r.total.c~outflow.1+logfund_size+logfund_age+period_return|code+DateQ)]
liner10<-data[,felm(r.total.c~outflow.1+logfund_size+logfund_age+alpha_capm|code+DateQ)]
liner11<-data[,felm(r.total.c~outflow.1+logfund_size+logfund_age+alpha_fama|code+DateQ)]
liner12<-data[,felm(r.total.c~outflow.1+logfund_size+logfund_age+rank|code+DateQ)]

stargazer(liner1,liner2,liner3,liner4,liner5,liner6,liner7,liner8,liner9,liner10,liner11,liner12,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//3.doc",add.lines =list(c("fund", "yes", "yes","yes","yes","yes","yes","yes","yes","yes", "yes", "yes","yes"), c("time", "yes","yes","yes","yes","yes","yes","yes","yes","yes", "yes", "yes","yes")))