data.net.1<-data.net.1[, L.net:=quantile(netflow.1,0.2,na.rm=TRUE)][
  , H.net:=quantile(netflow.1,0.8,na.rm=TRUE)]

L.net<-quantile(data.net.1[,netflow.1],0.2,na.rm=TRUE)
H.net<-quantile(data.net.1[,netflow.1],0.8,na.rm=TRUE)

data.net.1<-data.net[group.net==1,.SD][
  , L.net:=quantile(netflow.1,0.2,na.rm=TRUE)][
    , H.net:=quantile(netflow.1,0.8,na.rm=TRUE)][
      , group2.net:=ifelse(netflow.1<L.net,1,ifelse(netflow.1>H.net,3,2))]


liner1 <- data.net.1[,segmented(lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return), seg.Z = ~ netflow.1,psi=quantile(netflow.1,0.2))]
summary(liner1)

liner1 <- data.net.1[,segmented(lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return), seg.Z = ~ netflow.1, psi = c(20,80) )]

stargazer(liner1,type="html",out="C://Users//shenfan//Desktop//data//lm//XXX//1.doc")

summary(liner1)

fit =lm(wage¡«bs(age ,knots =c(25 ,40 ,60) ),data=Wage )

liner1 <- data.net.1[,lm(r.total.c~bs(netflow.1,knots = c(0.0187))+logfund_size+logfund_age+period_return)]

slope(liner1)


a<-as.list(c("0.327","0.0187"))

liner1 <- data.net[,segmented(lm(r.total.c~netflow.1+logfund_size+logfund_age+period_return), seg.Z = ~ netflow.1, psi = 0)]
summary(liner1)

