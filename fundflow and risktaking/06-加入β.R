#加入综合β，前10大股票占全部股票投资比，前5大行业占全部行业比， 重仓行业市值占股票投资市值比，重仓股市值占股票投资市值比(1)
load("datamain3.RData")

#综合β
#readbeta
beta <- read_excel("C://Users//shenfan//Desktop//data//2010-2017股票beta.xlsx")
setnames(beta, 1:34, c("code", "name", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
beta = melt(beta, id.vars = c("code", "name"), measure.vars = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
beta <- as.data.table(beta)
setnames(beta, 3, "date")
beta <- beta[, date := as.Date(as.character(date))
	][, id := (substring(code, 1, 6))
	][name != "NA"] #NA 是删掉wind的注释


#读取个股持仓
pf1 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//Fund_Portfolio_Stock.xlsx")
pf2 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//Fund_Portfolio_Stock1.xlsx")
pf3 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//Fund_Portfolio_Stock2.xlsx")
pf <- rbindlist(list(pf1, pf2, pf3))
rm(pf1, pf2, pf3)
pf <- as.data.table(pf) #pf为个股持仓原始文件，误删
#将数据变成portfolio
portfolio <- pf
portfolio <- as.data.table(portfolio)
#删除中报和年报
portfolio <- portfolio[ReportTypeID == 1 | ReportTypeID == 2 | ReportTypeID == 3 | ReportTypeID == 4, .SD]
#整理，包括去除不要的列和enddate改名
setnames(portfolio, "EndDate", "date")
setnames(portfolio, "Symbol", "id")
portfolio <- portfolio[, c("FundID", "ReportTypeID", "Startdate", "StockName", "InvestmentType", "Shares", "MarketValue") := NULL]
#让proportiond等变成数字
portfolio <- portfolio[, Proportion := as.numeric(as.character(Proportion))]
portfolio <- portfolio[, Rank := as.numeric(as.character(Rank))]
portfolio <- portfolio[, date := as.Date(as.character(date))]
#关于个股持仓的 chicang2是已经被删减完的了
portfolio <- portfolio[beta, on = .(id, date), nomatch = 0]
setnames(portfolio, "value", "beita")
portfolio <- portfolio[order(MasterFundCode, date, Rank)]
#删除beita是NA的项目
portfolio <- portfolio[, beitaTF := is.na(beita)]
portfolio <- portfolio[beitaTF == FALSE, .SD]
#删除proportion为0的
portfolio <- portfolio[Proportion!=0,.SD]
#proportion2是前十大股票的各自占比，zh为综合beta值
portfolio <- portfolio[, Proportion2 := Proportion / sum(Proportion), keyby = .(MasterFundCode, date)]
portfolio <- portfolio[, ji := Proportion2 * beita]
portfolio <- portfolio[, zh.bt := sum(ji), keyby = .(MasterFundCode, date)]
portfolio <- portfolio[,av.bt:=mean(beita),keyby=.(MasterFundCode, date)]
#删除重复项
portfolio <- portfolio[, yesno := (duplicated(portfolio, by = c("MasterFundCode", "date")))]
portfolio <- portfolio[yesno == FALSE, .SD]
#删除多余列，只要综合β
portfolio <- portfolio[, c("Rank", "id", "Proportion", "code", "name", "beita", "beitaTF", "Proportion2", "ji", "yesno") := NULL]
setnames(portfolio, "MasterFundCode", "id")
setnames(portfolio, 2, "DateQ")

add.data <- portfolio

#加入前10大股票占全部股票投资比
shenfan1 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//股票型 前10大股票占全部股票投资比.xlsx")
shenfan2 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//混合型 前10大股票占全部股票投资比.xlsx")
shenfan <- rbindlist(list(shenfan1, shenfan2))
shenfan <- as.data.table(shenfan)
setnames(shenfan, 1:34, c("code", "name","2010-03-31","2010-06-30","2010-09-30","2010-12-31","2011-03-31","2011-06-30","2011-09-30","2011-12-31","2012-03-31","2012-06-30","2012-09-30","2012-12-31","2013-03-31","2013-06-30","2013-09-30","2013-12-31","2014-03-31","2014-06-30","2014-09-30","2014-12-31","2015-03-31","2015-06-30","2015-09-30","2015-12-31","2016-03-31","2016-06-30","2016-09-30","2016-12-31","2017-03-31","2017-06-30","2017-09-30","2017-12-31"))
shenfan = melt(shenfan, id.vars = c("code", "name"), measure.vars = c("2010-03-31","2010-06-30","2010-09-30","2010-12-31","2011-03-31","2011-06-30","2011-09-30","2011-12-31","2012-03-31","2012-06-30","2012-09-30","2012-12-31","2013-03-31","2013-06-30","2013-09-30","2013-12-31","2014-03-31","2014-06-30","2014-09-30","2014-12-31","2015-03-31","2015-06-30","2015-09-30","2015-12-31","2016-03-31","2016-06-30","2016-09-30","2016-12-31","2017-03-31","2017-06-30","2017-09-30","2017-12-31"))
setnames(shenfan, "variable", "DateQ")
setnames(shenfan, 4, c("top.10.stock"))
shenfan <- shenfan[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD
	][, id := substring(code, 1, 6)
	][, code := NULL
	][, top.10.stock := top.10.stock / 100]

add.data <- add.data[shenfan, on = .(id, DateQ), nomatch = 0]

#加入股票型 前5大行业占全部行业比
shenfan1 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//股票型 前5大行业占全部行业比.xlsx")
shenfan2 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//混合型 前5大行业占全部行业比.xlsx")
shenfan <- rbindlist(list(shenfan1, shenfan2))
shenfan <- as.data.table(shenfan)
setnames(shenfan, 1:34, c("code", "name", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
shenfan = melt(shenfan, id.vars = c("code", "name"), measure.vars = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
setnames(shenfan, "variable", "DateQ")
setnames(shenfan, 4, c("top.industry"))
shenfan <- shenfan[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD
	][, id := substring(code, 1, 6)
	][, code := NULL
	][, top.industry := top.industry / 100]

add.data <- add.data[shenfan, on = .(id, DateQ), nomatch = 0]

#加入股票市值占基金资产净值比
shenfan1 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//股票型 股票市值占基金资产净值比.xlsx")
shenfan2 <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//混合型 股票市值占基金资产净值比.xlsx")
shenfan <- rbindlist(list(shenfan1, shenfan2))
shenfan <- as.data.table(shenfan)
setnames(shenfan, 1:34, c("code", "name", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
shenfan = melt(shenfan, id.vars = c("code", "name"), measure.vars = c("2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31"))
setnames(shenfan, "variable", "DateQ")
setnames(shenfan, 4, c("stock.proportion"))
shenfan <- shenfan[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][code != "NA"
	][code != "数据来源：Wind", .SD
	][, id := substring(code, 1, 6)
	][, code := NULL
	][, stock.proportion := stock.proportion / 100]

add.data <- add.data[shenfan, on = .(id, DateQ), nomatch = 0]

data.main31 <- add.data[data.main3, on = .(id, DateQ)]

data.main4<- add.data[data.main3, on = .(id, DateQ), nomatch=0] #这里又会删掉一些 比如004040这种，2017-8-1成立，但是有后面的数据，但是没有持仓的数据

#加入投资风格
style <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//style.xlsx")
style <- as.data.table(style)
setnames(style, 1:3, c("code", "name", "style"))
style <- style[, name := NULL]
data.main4 <- style[data.main4, on = .(code)]
data.main4 <- data.main4[id == "001622", style := "平衡风格型基金"
	][style == "平衡风格型基金", style := "mixed"
	][style == "价值风格型基金", style := "value"
	][style == "成长风格型基金", style := "growth"]

save(data.main4, file = "datamain4.RData")

rm(add.data, beta, style, shenfan, shenfan1, shenfan2, portfolio, pf, data.main3, data.main31, data.main4)