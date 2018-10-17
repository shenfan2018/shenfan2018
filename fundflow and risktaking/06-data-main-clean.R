#进行一些数据的整体改造 包括一些滞后，risk change，performance10等分，age三等分
load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datamain3.RData")

#调整数量级
data.main3<-data.main3[,netflow:=netflow/100][
  ,inflow:=inflow/100][
    , outflow:=outflow/100]

#fund flow的一些滞后
data.main3<-data.main3[order(id,DateQ)][
  , netflow.1:=shift(netflow,n=1,fill=NA,type="lag"),keyby=id][
    , netflow.2:=shift(netflow,n=2,fill=NA,type="lag"),keyby=id][
      , netflow.3:=shift(netflow,n=3,fill=NA,type="lag"),keyby=id][
        , netflow.4:=shift(netflow,n=4,fill=NA,type="lag"),keyby=id][
          , inflow.1:=shift(inflow,n=1,fill=NA,type="lag"),keyby=id][
            , inflow.2:=shift(inflow,n=2,fill=NA,type="lag"),keyby=id][
              ,inflow.3:=shift(inflow,n=3,fill=NA,type="lag"),keyby=id][
                , outflow.1:=shift(outflow,n=1,fill=NA,type="lag"),keyby=id][
                  ,outflow.2:=shift(outflow,n=2,fill=NA,type="lag"),keyby=id][
                    , outflow.3:=shift(outflow,n=3,fill=NA,type="lag"),keyby=id][
                      , period_return.1:=shift(period_return,n=1,fill=NA,type="lag"),keyby=id][
                        , logfund_age.1:=shift(logfund_age,n=1,fill=NA,type="lag"),keyby=id][
                          , logfund_size.1:=shift(logfund_size,n=1,fill=NA,type="lag"),keyby=id]

#risk 滞后
data.main3<-data.main3[order(id,DateQ)][
  ,r.total.1:=shift(risk.total,n=1,fill=NA,type="lag"),keyby=id][
    ,risk_capm.1:=shift(risk_capm,n=1,fill=NA,type="lag"),keyby=id][
      ,risk_fama.1:=shift(risk_fama,n=1,fill=NA,type="lag"),keyby=id]

#risk change 
data.main3<-data.main3[,r.total.c:=risk.total-r.total.1][
  , r.capm.c:=risk_capm-risk_capm.1][
    , r.fama.c:=risk_fama-risk_fama.1]

#risk change percent
data.main3<-data.main3[,r.total.c.p:=(risk.total-r.total.1)/r.total.1][
  , r.capm.c.p:=(risk_capm-risk_capm.1)/risk_capm.1][
    , r.fama.c.p:=(risk_fama-risk_fama.1)/risk_fama.1]

#加入rank
data.main3<-data.main3[order(DateQ,-period_return)][
  ,rank:=sequence(.N),by=.(DateQ)]

data.main4<-data.main3

save(data.main4,file="datamain4.RData")



