#NAV daily(NAVA为原始，NAV处理)
NAV1<-read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM.xlsx")
NAV2<-read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM2.xlsx")
NAV3<-read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM3.xlsx")
NAV4<-read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM4.xlsx")
NAV5<-read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM5.xlsx")
NAV6<-read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM6.xlsx")
NAV7<-read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM7.xlsx")
NAV<-rbindlist(list(NAV1,NAV2,NAV3,NAV4,NAV5,NAV6,NAV7))
rm(NAV1,NAV2,NAV3,NAV4,NAV5,NAV6,NAV7)
#NAV原始数据
data.NAV<-NAV
data.NAV<-as.data.table(data.NAV)
setnames(data.NAV,1:2,c("date","id"))
data.NAV <- data.NAV[date != "没有单位'", .SD
	][date != "统计日期'", .SD
	][, AdjustedNAV := as.numeric(as.character(AdjustedNAV))
	][, AdjustedNAVGrowth := as.numeric(as.character(AdjustedNAVGrowth))
	][, date := as.Date(as.character(date))
	][, c("FullName", "NAV") := NULL]

#根据adjustedNAV自己算return,这里的不要百分号，最简单的形式
#排序
data.NAV <- data.NAV[order(id, date)
	][, AdjustedNAV1 := shift(AdjustedNAV, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][, return.daily := (log(AdjustedNAV) - log(AdjustedNAV1))
	][, quarter := quarter(date)
	][, year := year(date)
	][, risk.total := sd((return.daily), na.rm = TRUE), by = .(quarter, year, id)
	][, period_return := (log(AdjustedNAV[.N]) - log(AdjustedNAV[1])), keyby = .(quarter, year, id)]

data.capm<-data.NAV[,c("id","date","quarter","year","return.daily","risk.total","period_return")]

#加入riskfree
riskfree<-read_excel("C://Users//shenfan//Desktop//data//CAPM//risk-free.xlsx")
riskfree<-as.data.table(riskfree)
setnames(riskfree,1,"date")
setnames(riskfree,2,"risk_free")
riskfree <- riskfree[, date := as.Date(date)
	][, risk_free.daily := (risk_free + 1) ^ (1 / 365) - 1]

add<-riskfree[,c("date","risk_free.daily")]
#riskfree匹配上主表
data.capm <- add[data.capm, on = .(date), nomatch = 0]

#market risk
marketrisk<-read_excel("C://Users//shenfan//Desktop//data//CAPM//综合日市场回报率文件2//TRD_Cndalym.xlsx")
marketrisk<-as.data.table(marketrisk)
#选择type
marketrisk<-marketrisk[Markettype=="5"][,Markettype:=NULL]

setnames(marketrisk,2,"market_return") #考虑现金红利的综合日市场回报率(流通市值加权平均法)'
setnames(marketrisk,1,"date")
marketrisk<-marketrisk[date!="没有单位'",.SD][date!="交易日期'",.SD][
  ,date:=as.Date(date)][
    ,market_return:=as.numeric(as.character(market_return))]

#匹配market risk
data.capm <- marketrisk[data.capm, on = .(date), nomatch = 0]



#计算调整后的收益（减去市场风险的）
data.capm <- data.capm[, return.adj.daily := return.daily - market_return
	][, risk.adj.total := sd((return.adj.daily), na.rm = TRUE), by = .(quarter, year, id)]

#删掉year和quarter
data.capm[, c("year", "quarter") := NULL]

#fund style(wind+csmar)
fund.csmar<-read_excel("C://Users//shenfan//Desktop//data//基金类型风格//Fund_MainInfo.xlsx")
fund.csmar<-as.data.table(fund.csmar)
setnames(fund.csmar,1:2,c("id","name"))
fund.csmar<-fund.csmar[name!="基金全称'",.SD][
  name!="没有单位'",.SD][
    ,name:=NULL]

fund.wind<-read_excel("C://Users//shenfan//Desktop//data//基金类型风格//windstyle.xlsx")
fund.wind<-as.data.table(fund.wind)
setnames(fund.wind,1:6,c("code","name","category2","category1","style1","style2"))
fund.wind<-fund.wind[,id:=substring(code,1,6)][
  ,c("style1","style2","name","code","category1"):=NULL]
fund.category<-fund.csmar[fund.wind,on=.(id)]
#match
data.capm<-fund.category[data.capm,on=.(id),nomatch=0]

#选择混合型和股票型
data.capm<-data.capm[Category=="股票型基金"|Category=="混合型基金",.SD][
  category2=="偏股混合型基金"|category2=="普通股票型基金",.SD]

#就算capm的东西
data.capm<-data.capm[,riskpreimum:=market_return-risk_free.daily]
data.capm<-data.capm[,rit:=return.daily-risk_free.daily]

rm(data.NAV,marketrisk,riskfree,NAV,add,fund.csmar,fund.wind,fund.category)
save(data.capm,file="datacapm.RData")
