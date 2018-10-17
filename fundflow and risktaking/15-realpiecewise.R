load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain4.RData")

data<-data.main4

#这个要先删了
a1<-quantile(data[,netflow.1],0.99,na.rm=TRUE)
a2<-quantile(data[,netflow.1],0.01,na.rm=TRUE)
data<-data[,extremum:=ifelse(netflow.1>a1|netflow.1<a2,1,0)][
  extremum=="0",.SD]

liner1 <- data[,segmented(lm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1), seg.Z = ~ netflow.1, psi=c(quantile(netflow.1,0.2),quantile(netflow.1,0.8)))]
coef(liner1)[2]

#felm
summary(liner1<-data[,felm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1|code+DateQ)])
#plm
summary(plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,index=c("DateQ","id")))
summary(plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,model="pooling",index=c("DateQ","id")))
summary(plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,effect="time",model="within",index=c("DateQ","id")))

summary(plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,effect="time",index=c("DateQ"))) #R方太小了

segmented(plm(r.total.c~netflow.1+logfund_size.1+logfund_age.1+period_return.1,data,index=c("DateQ","id")))

fit.lm<-data[,lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return+factor(DateQ))]
liner1 <- data[,segmented(lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return+factor(DateQ)), seg.Z = ~ netflow.1, psi=c(quantile(netflow.1,0.2),quantile(netflow.1,0.8)))]
fit.lm<-update(fit.lm,.~.-netflow.1)
liner1<-update(liner1)
summary(liner1)


coef(data[,segmented(lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return), seg.Z = ~ netflow.1, psi=c(quantile(netflow.1,0.2),quantile(netflow.1,0.8)))])

data<-data[,netflow.1.c:=coef(segmented(lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return), seg.Z = ~ netflow.1, psi=c(quantile(netflow.1,0.2),quantile(netflow.1,0.8))))[2],keyby=.(DateQ)]

data<-data[,netflow.1.c:=coef(segmented(lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return), seg.Z = ~ netflow.1, psi=c(quantile(netflow.1,0.2),quantile(netflow.1,0.8))))[2]]

slope(liner1)
summary(liner1)

liner1 <- data[,segmented(lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return), seg.Z = ~ netflow.1,psi=c(quantile(netflow.1,0.2),quantile(netflow.1,0.8))),by=DateQ]

a<-data[,quantile(netflow.1,0.2),by=DateQ]
b<-data[,quantile(netflow.1,0.8),by=DateQ]

liner1 <- data[,segmented(lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return+as.factor(a)+as.factor(b)), seg.Z = ~ netflow.1,psi=c(quantile(netflow.1,0.2),quantile(netflow.1,0.8)))]
