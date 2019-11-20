load("datacapm.RData")
load("datamain.RData")

#回归残差 aplha beta
data.capm <- data.capm[, year := year(date)
	][, quarter := quarter(date)
	][return.daily != "NA", .SD
	][, rd := residuals(lm(rit ~ riskpreimum)), keyby = .(id, year, quarter)
	][, alpha_capm := coef(lm(rit ~ riskpreimum))[1], keyby = .(id, year, quarter)
	][, beta_capm := coef(lm(rit ~ riskpreimum))[2], keyby = .(id, year, quarter)
	][,risk_capm:=sd((rd),na.rm=TRUE),keyby=.(id,year,quarter)]

data.capmQ<-data.capm
#删除重复项(也就是daily变成quarter)
data.capmQ <- data.capmQ[, yesno := (duplicated(data.capmQ, by = c("id", "quarter", "year")))
	][yesno == FALSE, .SD
	][,c("id","risk.total","year","quarter","alpha_capm","beta_capm","risk_capm","period_return","risk.adj.total","return.adj.daily")]

#capm系列加入data.main
data.main2<-data.main
data.main2<-data.main2[,year:=year(DateQ)][
  ,quarter:=quarter(DateQ)]
data.main2<-data.capmQ[data.main2,on=.(quarter,year,id),nomatch=0]

save(data.main2,file="datamain2.RData")

rm(data.capmQ,data.capm,data.main2)
