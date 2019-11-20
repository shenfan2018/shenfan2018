#傻逼的整理
#下载的权重
a1 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20100630.xlsx")
a1 <- as.data.table(a1)
a1 <- a1[, Date := as.Date("2010-06-30")]
a2 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20101231.xlsx")
a2 <- as.data.table(a2)
a2 <- a2[, Date := as.Date("2010-12-31")]
a3 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20110630.xlsx")
a3 <- as.data.table(a3)
a3 <- a3[, Date := as.Date("2011-06-30")]
a4 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20111231.xlsx")
a4 <- as.data.table(a4)
a4 <- a4[, Date := as.Date("2011-12-31")]
a5 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20120630.xlsx")
a5 <- as.data.table(a5)
a5 <- a5[, Date := as.Date("2012-06-30")]
a6 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20121231.xlsx")
a6 <- as.data.table(a6)
a6 <- a6[, Date := as.Date("2012-12-31")]
a7 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20130630.xlsx")
a7 <- as.data.table(a7)
a7 <- a7[, Date := as.Date("2013-06-30")]
a8 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20131231.xlsx")
a8 <- as.data.table(a8)
a8 <- a8[, Date := as.Date("2013-12-31")]
a9 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20140630.xlsx")
a9 <- as.data.table(a9)
a9 <- a9[, Date := as.Date("2014-06-30")]
a10 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20141231.xlsx")
a10 <- as.data.table(a10)
a10 <- a10[, Date := as.Date("2014-12-31")]
a11 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20150630.xlsx")
a11 <- as.data.table(a11)
a11 <- a11[, Date := as.Date("2015-06-30")]
a12 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20151231.xlsx")
a12 <- as.data.table(a12)
a12 <- a12[, Date := as.Date("2015-12-31")]
a13 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20160630.xlsx")
a13 <- as.data.table(a13)
a13 <- a13[, Date := as.Date("2016-06-30")]
a14 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20161231.xlsx")
a14 <- as.data.table(a14)
a14 <- a14[, Date := as.Date("2016-12-31")]
a15 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20170630.xlsx")
a15 <- as.data.table(a15)
a15 <- a15[, Date := as.Date("2017-06-30")]
a16 <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//sz20171231.xlsx")
a16 <- as.data.table(a16)
a16 <- a16[, Date := as.Date("2017-12-31")]

quanzhong <- rbindlist(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))

setnames(quanzhong, 2:4, c("id", "name", "proportion"))
quanzhong <- quanzhong[, c("id", "name", "Date", "proportion")]

write.csv(quanzhong, "C://Users//shenfan//Desktop//data//指数成分股权重//szzhengli.csv")

rm(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

# 股价整理 为2010-2017的日度数据(增加了2009.7.1-,又增加了2018.1.1-2018.6.30）
#stock daily
stock0 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr+.xlsx")
stock1 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr.xlsx")
stock2 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr1.xlsx")
stock3 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr2.xlsx")
stock4 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr3.xlsx")
stock5 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr4.xlsx")
stock6 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr5.xlsx")
stock7 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr++.xlsx")
stock.raw <- rbindlist(list(stock0,stock1, stock2, stock3, stock4, stock5, stock6,stock7)) #stock原文件 勿删
rm(stock0,stock1, stock2, stock3, stock4, stock5, stock6,stock7)
stock <- stock.raw
stock <- as.data.table(stock)
setnames(stock, c("Trddt", "Stkcd"), c("date", "stock.id"))
stock <- stock[, c("stock.id", "date", "Clsprc", "Dretwd")]

#整理
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, date := as.Date(date)]

save(stock, file = "datastock.RData")


##
#NAV daily(NAVA为原始，NAV处理)
NAV0 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM0406.xlsx")
NAV00 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM0608.xlsx")
NAV000 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM0810.xlsx")
NAV1 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM1.xlsx")
NAV2 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM2.xlsx")
NAV3 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM3.xlsx")
NAV4 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM4.xlsx")
NAV5 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM5.xlsx")
NAV6 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM6.xlsx")
NAV7 <- read_excel("C://Users//shenfan//Desktop//data//业绩 净值//净值表现复权//Fund_NAV_PFM7.xlsx")
NAV <- rbindlist(list(NAV0, NAV00, NAV000, NAV1, NAV2, NAV3, NAV4, NAV5, NAV6, NAV7))
rm(NAV0, NAV00, NAV000, NAV1, NAV2, NAV3, NAV4, NAV5, NAV6, NAV7)
#NAV原始数据
data.NAV <- NAV
data.NAV <- as.data.table(data.NAV)
setnames(data.NAV, 1:2, c("date", "id"))
data.NAV <- data.NAV[date != "没有单位'", .SD
	][date != "统计日期'", .SD
	][, AdjustedNAV := as.numeric(as.character(AdjustedNAV))
	][, AdjustedNAVGrowth := as.numeric(as.character(AdjustedNAVGrowth))
	][, date := as.Date(as.character(date))
	][, c("FullName", "NAV") := NULL]

#根据adjustedNAV自己算return,这里的收益率都是%，都*100
#排序
data.NAV <- data.NAV[order(id, date)
	][, AdjustedNAVGrowth := AdjustedNAVGrowth / 100]

save(data.NAV, file = "fund-NAV.RData")
	
