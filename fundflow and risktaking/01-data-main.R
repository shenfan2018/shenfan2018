#以fundflow做一个主表data.main
fundflow1<-read_excel("C://Users//shenfan//Desktop//data//股票型混合型//股票型 单季度净申购赎回率.xlsx")
fundflow2<-read_excel("C://Users//shenfan//Desktop//data//股票型混合型//混合型 单季度净申购赎回率.xlsx")
fundflow<-rbindlist(list(fundflow1,fundflow2))
fundflow<-as.data.table(fundflow)
setnames(fundflow,1:34,c("code","name","2010-03-31","2010-06-30","2010-09-30","2010-12-31","2011-03-31","2011-06-30","2011-09-30","2011-12-31","2012-03-31","2012-06-30","2012-09-30","2012-12-31","2013-03-31","2013-06-30","2013-09-30","2013-12-31","2014-03-31","2014-06-30","2014-09-30","2014-12-31","2015-03-31","2015-06-30","2015-09-30","2015-12-31","2016-03-31","2016-06-30","2016-09-30","2016-12-31","2017-03-31","2017-06-30","2017-09-30","2017-12-31"))
fundflow=melt(fundflow,id.vars=c("code","name"),measure.vars=c("2010-03-31","2010-06-30","2010-09-30","2010-12-31","2011-03-31","2011-06-30","2011-09-30","2011-12-31","2012-03-31","2012-06-30","2012-09-30","2012-12-31","2013-03-31","2013-06-30","2013-09-30","2013-12-31","2014-03-31","2014-06-30","2014-09-30","2014-12-31","2015-03-31","2015-06-30","2015-09-30","2015-12-31","2016-03-31","2016-06-30","2016-09-30","2016-12-31","2017-03-31","2017-06-30","2017-09-30","2017-12-31"))
setnames(fundflow,"variable","DateQ")
setnames(fundflow,4,c("netflow.wind"))
fundflow<-fundflow[,DateQ:=as.Date(as.character(DateQ))][
  ,name:=NULL][
    code!="NA"][
      code!="数据来源：Wind",.SD][
      ,id:=substring(code,1,6)]

data.main<-fundflow

#加入csmar的inflow outflow
sharechange<-read_excel("C://Users//shenfan//Desktop//data//股票型混合型//Fund_ShareChange.xlsx")
sharechange<-as.data.table(sharechange)
setnames(sharechange,2:11,c("class","id","type","start","DateQ","beginning","purchase","redemption","split","end"))
sharechange <- sharechange[id != "基金代码'", .SD
	][id != "没有单位'"
	][, type := as.numeric(as.character(type))
	][type == 1 | type == 2 | type == 3 | type == 4, .SD
	][, DateQ := as.Date(as.character(DateQ))
	][, beginning := as.numeric(as.character(beginning))
	][, purchase := as.numeric(as.character(purchase))
	][, redemption := as.numeric(as.character(redemption))
	][, end := as.numeric(as.character(end))
	][, inflow := 100 * purchase / beginning
	][, outflow := 100 * redemption / beginning
	][, netflow := 100 * (purchase - redemption) / beginning]
                   
add<-sharechange[,c("id","DateQ","inflow","outflow","netflow")]

data.main<-add[data.main,on=.(id,DateQ),nomatch=0]

#添加市值
marketvalue1<-read_excel("C://Users//shenfan//Desktop//data//股票型混合型//股票型 基金资产净值.xlsx")
marketvalue2<-read_excel("C://Users//shenfan//Desktop//data//股票型混合型//混合型 基金资产净值.xlsx")
marketvalue<-rbindlist(list(marketvalue1,marketvalue2))
marketvalue<-as.data.table(marketvalue)
setnames(marketvalue,1:34,c("code","name","2010-03-31","2010-06-30","2010-09-30","2010-12-31","2011-03-31","2011-06-30","2011-09-30","2011-12-31","2012-03-31","2012-06-30","2012-09-30","2012-12-31","2013-03-31","2013-06-30","2013-09-30","2013-12-31","2014-03-31","2014-06-30","2014-09-30","2014-12-31","2015-03-31","2015-06-30","2015-09-30","2015-12-31","2016-03-31","2016-06-30","2016-09-30","2016-12-31","2017-03-31","2017-06-30","2017-09-30","2017-12-31"))
marketvalue=melt(marketvalue,id.vars=c("code","name"),measure.vars=c("2010-03-31","2010-06-30","2010-09-30","2010-12-31","2011-03-31","2011-06-30","2011-09-30","2011-12-31","2012-03-31","2012-06-30","2012-09-30","2012-12-31","2013-03-31","2013-06-30","2013-09-30","2013-12-31","2014-03-31","2014-06-30","2014-09-30","2014-12-31","2015-03-31","2015-06-30","2015-09-30","2015-12-31","2016-03-31","2016-06-30","2016-09-30","2016-12-31","2017-03-31","2017-06-30","2017-09-30","2017-12-31"))
setnames(marketvalue,c("value","variable"),c("fund_size","DateQ"))
marketvalue<-marketvalue[,DateQ:=as.Date(as.character(DateQ))][
  ,name:=NULL][
    ,logfund_size:=as.data.table(log(fund_size))][
      order(code,DateQ)]

data.main<-marketvalue[data.main,on=.(code,DateQ),nomatch=0]

#添加fund_age
fundage<-read_excel("C://Users//shenfan//Desktop//data//股票型混合型//成立日期.xlsx")
fundage<-as.data.table(fundage)
setnames(fundage,1:2,c("id","startdate"))
fundage<-fundflow[fundage,on=.(id),nomatch=0]
fundage<-fundage[,enddate:=DateQ]
#计算fund_age
fundage<-fundage[,days:=difftime(enddate,startdate,units=c("days"))][
  ,fund_age:=days/365][
    ,fund_age:=as.numeric(fund_age)][
      ,logfund_age:=log(fund_age)]
add<-fundage[,c("code","DateQ","fund_age","logfund_age")]
data.main<-add[data.main,on=.(code,DateQ),nomatch=0]

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
data.main<-fund.category[data.main,on=.(id),nomatch=0]

#选择混合型和股票型
data.main<-data.main[Category=="股票型基金"|Category=="混合型基金",.SD][
  category2=="偏股混合型基金"|category2=="普通股票型基金",.SD]
#这里先将csmar上net flow没有的直接剔除了哦
data.main<-data.main[netflow!="NA",.SD]

save(data.main,file="datamain.RData")

rm(fundflow1,fundflow2,fundflow,fundage,marketvalue,marketvalue1,marketvalue2,add,sharechange,fundage1,fundage2,fund.wind,fund.csmar,fund.category,data.main)