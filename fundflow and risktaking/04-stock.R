#FAMA three factor
#stock daily
stock0 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr+.xlsx")
stock1 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr.xlsx")
stock2 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr1.xlsx")
stock3 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr2.xlsx")
stock4 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr3.xlsx")
stock5 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr4.xlsx")
stock6 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr5.xlsx")
stock7 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr++.xlsx")
stock.raw <- rbindlist(list(stock0, stock1, stock2, stock3, stock4, stock5, stock6, stock7)) #stock原文件 勿删
rm(stock0, stock1, stock2, stock3, stock4, stock5, stock6, stock7)
stock <- stock.raw
stock <- as.data.table(stock)
setnames(stock, c("Trddt", "Stkcd", "Dsmvosd"), c("date", "code", "circulation_ME"))

#整理
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, circulation_ME := as.numeric(as.character(circulation_ME))
	][, date := as.Date(date)
	][, quarter := quarter(date)
	][, year := year(date)
	][,c("Dsmvtll","Dretnd"):=NULL]

#BE
BE<-read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//股东权益合计.xlsx")
BE <- as.data.table(BE)
setnames(BE, 1:34, c("code", "name", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
BE <- melt(BE, id.vars = c("code", "name"), measure.vars = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
setnames(BE, c("variable", "value"), c("date", "owners_equity"))
BE <- BE[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, code := (substring(BE[, code], 1, 6))
	][, owners_equity := as.numeric(owners_equity)
	][, c("code", "owners_equity", "year", "quarter")]

stock <- BE[stock, on = .(code, year, quarter)]
#就算BE/ME
stock <- stock[, BEME := owners_equity / circulation_ME]

#目前的都是日度的
#选择HML和SMB
stock <- stock[order(code, date)
	][owners_equity != "NA", .SD]

#S B 分组
stock<-stock[,M:=ntile(circulation_ME,2),by=.(date)]
#L M H 分组
stock<-stock[,L:=quantile(BEME,0.3,na.rm=TRUE),by=date][
  ,H:=quantile(BEME,0.7,na.rm=TRUE),by=date][
    ,BEME2:=ifelse(BEME<L,1,ifelse(BEME>H,3,2))]

# 普通平均
stock<-stock[,SMB_return:=mean(Dretwd),by=.(date,M)][
  ,HML_return:=mean(Dretwd),by=.(date,BEME2)]

#计算SMB
stock<-stock[order(M),.SD,keyby=.(date)][
  ,SMB:=SMB_return[1]-SMB_return[.N],keyby=.(date)][
    order(BEME2),.SD,keyby=.(date)][
      ,HML:=HML_return[1]-HML_return[.N],keyby=.(date)]

save(stock,file="stock.RData")

#提取因子删重复的变成日度
stockQ<-stock[,c("date","SMB","HML")][
  ,yesno:=(duplicated(SMB,by=.(date)))][
    order(date)][
      yesno==FALSE,.SD][
        ,yesno:=NULL]

data.fama<-stockQ[data.capm,on=.(date),nomatch=0]

rm(BE,stock.raw,stockQ)

save(data.fama,file="datafama.RData")
