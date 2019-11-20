# 股价整理 为2010-2017的日度数据(2003.1.1-2019.3.31)
#stock daily
stock0 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr0.xlsx")
stock1 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr1.xlsx")
stock2 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr2.xlsx")
stock3 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr3.xlsx")
stock4 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr4.xlsx")
stock5 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr5.xlsx")
stock6 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr6.xlsx")
stock7 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr7.xlsx")
stock8 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr8.xlsx")
stock9 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr9.xlsx")
stock10 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr10.xlsx")
stock11 <- read_excel("C://Users//shenfan//Desktop//data//Carhat four factors//个股日度回报率//TRD_Dalyr11.xlsx")
stock.raw <- rbindlist(list(stock0, stock1, stock2, stock3, stock4, stock5, stock6, stock7, stock8, stock9, stock10, stock11)) #stock原文件 勿删
rm(stock0, stock1, stock2, stock3, stock4, stock5, stock6, stock7, stock8, stock9, stock10, stock11)
stock <- stock.raw
stock <- as.data.table(stock)
setnames(stock, c("Trddt", "Stkcd"), c("date", "stock.id"))
stock <- stock[, c("stock.id", "date", "Clsprc", "Dretwd", "Dsmvtll")]

stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, date := as.Date(date)
	][, Dsmvtll := as.numeric(Dsmvtll)]

save(stock, file = "stock20030101-20190331.RData")