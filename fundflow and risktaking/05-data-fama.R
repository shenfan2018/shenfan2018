load("C://Users//shenfan//Desktop//data//project//fundflow and risktaking//datafama.RData")

# 回归残差 aplha beta
data.fama<-data.fama[,rd3:=residuals(lm(rit~riskpreimum+SMB+HML)), keyby = .(id,year,quarter)][
  ,alpha_fama:=coef(lm(rit~riskpreimum+SMB+HML))[1], keyby = .(id,year,quarter)][
    ,beta_fama:=coef(lm(rit~riskpreimum+SMB+HML))[2], keyby = .(id,year,quarter)][
      ,risk_fama:=sd((rd3),na.rm=TRUE),keyby=.(id,year,quarter)]

data.famaQ<-data.fama
#删除重复项(也就是daily变成quarter)
data.famaQ<-data.famaQ[,yesno:=(duplicated(risk_fama,by=c("id","quarter","year")))][
  yesno==FALSE,.SD][
    ,c("id","year","quarter","alpha_fama","beta_fama","risk_fama")]

data.main3<-data.famaQ[data.main2,on=.(id,year,quarter),nomatch=0]

save(data.main3,file="datamain3.RData")

rm(data.famaQ,data.fama,data.main3)


