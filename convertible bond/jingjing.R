stock1 <- read_excel("C:/Users/shenfan/Desktop/可转债/日个股回报率文件/TRD_Dalyr.xlsx")
stock2 <- read_excel("C:/Users/shenfan/Desktop/可转债/日个股回报率文件/TRD_Dalyr1.xlsx")
stock3 <- read_excel("C:/Users/shenfan/Desktop/可转债/日个股回报率文件/TRD_Dalyr2.xlsx")

stock.raw <- rbindlist(list(stock1, stock2, stock3)) #stock原文件 勿删
rm(stock1, stock2, stock3)

stock <- stock.raw
setnames(stock, 1:4, c("stock.id", "date", "open", "close"))

stock <- stock[!stock.id == "证券代码'"
	][!stock.id == "没有单位'"
	][, open := as.numeric(open)
	][, close := as.numeric(close)
	][, date := as.Date(date)]

a <- stock[order(stock.id, date)
	][, close2 := shift(close, n = 1, fill = NA, type = "lag"), keyby = stock.id
	][, ret := (open - close2) / close2
	][, ret2 := shift(ret, n = 1, fill = NA, type = "lead"), keyby = stock.id]

a <- a[, .(stock.id, date, close, ret2)]

bond <- read_excel("C:/Users/shenfan/Desktop/可转债/convertible bond.xlsx")
bond <- as.data.table(bond)
bond2 <-bond[,c("债券代码","正股代码","原股东配售股权登记日","每股配售额     " ,"上市时间" )]

setnames(bond2,1:3,c("bond.id","stock.id","date"))

#match
bond2 <- bond2[, stock.id := as.character(stock.id)][, date := as.Date(date)]
b <-a[bond2,on=.(stock.id,date)]

