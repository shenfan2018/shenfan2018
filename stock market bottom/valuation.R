#这里开始
szindex <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//指数行情序列.xlsx")
szindex <- as.data.table(szindex)
setnames(szindex, 1:10, c("date", "Clp", "volume", "volume2", "turnover", "turnover2", "circulateA", "PE", "PETTM", "PB"))
szindex <- szindex[Clp < 2600, .SD]
szindex2 <- szindex
szindex <- szindex2
szindex <- szindex[date > "2010-01-01", .SD]

#分别的进行排名位比
szindex <- szindex[order(volume), .SD
	][, volumepm := sequence(.N) / (.N)
	][order(volume2), .SD][, volume2pm := sequence(.N) / (.N)
	][order(turnover), .SD
	][, turnoverpm := sequence(.N) / (.N)
	][order(turnover2), .SD
	][, turnover2pm := sequence(.N) / (.N)
	][order(PE), .SD][, PEpm := sequence(.N) / (.N)
	][order(PETTM), .SD
	][, PETTMpm := sequence(.N) / (.N)
	][order(PB), .SD][, PBpm := sequence(.N) / (.N)
	][, date := as.Date(as.character(date))]


stock1 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr.xlsx")
stock2 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr1.xlsx")
stock3 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr2.xlsx")
stock4 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr3.xlsx")
stock5 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr4.xlsx")
stock6 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr5.xlsx")
stock7 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr6.xlsx")
stock8 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr7.xlsx")
stock9 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr8.xlsx")
stock10 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr9.xlsx")
stock11 <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//个股衍生//STK_MKT_Dalyr10.xlsx")
stock.raw <- rbindlist(list(stock1, stock2, stock3, stock4, stock5, stock6, stock7, stock8, stock9, stock10, stock11))
#stock原文件 勿删rm(stock1,stock2,stock3,stock4,stock5,stock6,stock7,stock8,stock9,stock10,stock11)
stock <- stock.rawstock <- as.data.table(stock)
setnames(stock,1,"date")
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, PE := as.numeric(as.character(PE))
	][, PB := as.numeric(as.character(PB))
	][, PCF := as.numeric(as.character(PCF))
	][, PS := as.numeric(as.character(PS))
	][, Turnover := as.numeric(as.character(Turnover))
	][, CirculatedMarketValue := as.numeric(as.character(CirculatedMarketValue))
	][, Amount := as.numeric(as.character(Amount))
	][, Liquidility := as.numeric(as.character(Liquidility))
	][, date := as.Date(as.character(date))]

shangshi <- read_excel("C://Users//shenfan//Desktop//杂//关于底部//上市的A股.xlsx")
shangshi <- as.data.table(shangshi)
setnames(shangshi, 1, "code")
setnames(stock, 2, "code")
stock2 <- shangshi[stock, on = .(code), nomatch = 0]

#加入行业
hangye<-read_excel("C://Users//shenfan//Desktop//杂//关于底部//行业.xlsx")
hangye <- as.data.table(hangye)
chuli <- szindex[stock2, on = .(date), nomatch = 0]

chuli <- chuli[, i.PE.avg := mean(i.PE, na.rm = TRUE), keyby = .(date)
	][, proportion := CirculatedMarketValue / sum(CirculatedMarketValue), keyby = .(date)
	][, i.PE.c := i.PE * proportion
	][, i.PE.c.avg := sum(i.PE.c, na.rm = TRUE), keyby = .(date)]

chuli <- chuli [, i.PB.avg := mean(i.PB, na.rm = TRUE), keyby = .(date)
][, proportion := CirculatedMarketValue / sum(CirculatedMarketValue), keyby = .(date)
][, i.PB.c := i.PB * proportion
][, i.PB.c.avg := sum(i.PB.c, na.rm = TRUE), keyby = .(date) ]

chuli <- chuli[, PCF.avg := mean(PCF, na.rm = TRUE), keyby = .(date)
	][, proportion := CirculatedMarketValue / sum(CirculatedMarketValue), keyby = .(date)
	][, PCF.c := PCF * proportion
	][, PCF.c.avg := sum(PCF.c, na.rm = TRUE), keyby = .(date)]

chuli <- chuli[, PS.avg := mean(PS, na.rm = TRUE), keyby = .(date)][, proportion := CirculatedMarketValue / sum(CirculatedMarketValue), keyby = .(date)][, PS.c := PS * proportion][, PS.c.avg := sum(PS.c, na.rm = TRUE), keyby = .(date)]
chuli <- chuli[, yesno := (duplicated(i.PE.avg)), by = date]
chuli <- chuli[yesno == FALSE, .SD]
chuli <- chuli[, c("i.PE.c", "i.PB.c", "PCF.c", "PS.c") := NULL]

#计算八项指标的排名
chuli <- chuli[order(i.PE.avg), .SD
	][, i.PE.avg.pm := sequence(.N) / (.N)
	][order(i.PE.c.avg), .SD
	][, i.PE.c.avg.pm := sequence(.N) / (.N)
	][order(i.PB.avg), .SD
	][, i.PB.avg.pm := sequence(.N) / (.N)
	][order(i.PB.c.avg), .SD
	][, i.PB.c.avg.pm := sequence(.N) / (.N)
	][order(PCF.avg), .SD
	][, PCF.avg.pm := sequence(.N) / (.N)
	][order(PCF.c.avg), .SD][, PCF.c.avg.pm := sequence(.N) / (.N)
	][order(PS.avg), .SD
	][, PS.avg.pm := sequence(.N) / (.N)
	][order(PS.c.avg), .SD
	][, PS.c.avg.pm := sequence(.N) / (.N)]

#标准化
chuli <- chuli[, i.PE.avg.bzh := (i.PE.avg - min(i.PE.avg)) / (max(i.PE.avg) - min(i.PE.avg))
	][, i.PE.c.avg.bzh := (i.PE.c.avg - min(i.PE.c.avg)) / (max(i.PE.c.avg) - min(i.PE.c.avg))
	][, i.PB.avg.bzh := (i.PB.avg - min(i.PB.avg)) / (max(i.PB.avg) - min(i.PB.avg))
	][, i.PB.c.avg.bzh := (i.PB.c.avg - min(i.PB.c.avg)) / (max(i.PB.c.avg) - min(i.PB.c.avg))
	][, PCF.avg.bzh := (PCF.avg - min(PCF.avg)) / (max(PCF.avg) - min(PCF.avg))
	][, PCF.c.avg.bzh := (PCF.c.avg - min(PCF.c.avg)) / (max(PCF.c.avg) - min(PCF.c.avg))
	][, PS.avg.bzh := (PS.avg - min(PS.avg)) / (max(PS.avg) - min(PS.avg))
	][, PS.c.avg.bzh := (PS.c.avg - min(PS.c.avg)) / (max(PS.c.avg) - min(PS.c.avg))]

#简单平均
chuli <- chuli[, gzyz := 0.25 * i.PE.avg.bzh + 0.25 * i.PB.avg.bzh + 0.25 * PCF.avg.bzh + 0.25 * PS.avg.bzh
	][order(gzyz), .SD
	][, gzyz.pm := sequence(.N)
	][, gzyz.wb := sequence(.N) / (.N)]

#加权平均
chuli <- chuli[, gzyz := 0.25 * i.PE.c.avg.bzh + 0.25 * i.PB.c.avg.bzh + 0.25 * PCF.c.avg.bzh + 0.25 * PS.c.avg.bzh
	][order(gzyz), .SD
	][, gzyz.pm := sequence(.N)
	][, gzyz.wb := sequence(.N) / (.N)]

#总
chuli <- chuli[, gzyz := 0.125 * i.PE.avg.bzh + 0.125 * i.PE.c.avg.bzh + 0.125 * i.PB.avg.bzh + 0.125 * i.PB.c.avg.bzh + 0.125 * PCF.avg.bzh + 0.125 * PCF.c.avg.bzh + 0.125 * PS.avg.bzh + 0.125 * PS.c.avg.bzh
    ][order(gzyz), .SD
    ][, gzyz.pm := sequence(.N)
    ][, gzyz.wb := sequence(.N) / (.N)]

chuli <- chuli[order(date)]
chuli <- chuli[, gzyz2 := Clp[1] / gzyz[1] * gzyz]
chuli <- chuli[, gzyz.pm.1 := shift(gzyz.pm, n = 1, fill = NA, type = "lag")]
chuli <- chuli[, cha := gzyz.pm - gzyz.pm.1]

#总
chuli <- chuli[, gzyz := 0.125 * i.PE.avg + 0.125 * i.PE.c.avg + 0.125 * i.PB.avg + 0.125 * i.PB.c.avg + 0.125 * PCF.avg + 0.125 * PCF.c.avg + 0.125 * PS.avg + 0.125 * PS.c.avg
	][order(gzyz), .SD
	][, gzyz.pm := sequence(.N)
	][, gzyz.wb := sequence(.N) / (.N)]

#简单平均
chuli <- chuli[, gzyz := 0.25 * i.PE.avg + 0.25 * i.PB.avg + +0.25 * PCF.avg + 0.25 * PS.avg
	][order(gzyz), .SD
	][, gzyz.pm := sequence(.N)
	][, gzyz.wb := sequence(.N) / (.N)]

#加权平均
chuli <- chuli[, gzyz := 0.25 * i.PE.c.avg + 0.25 * i.PB.c.avg + 0.25 * PCF.c.avg + 0.25 * PS.c.avg
	][order(gzyz), .SD
	][, gzyz.pm := sequence(.N)
	][, gzyz.wb := sequence(.N) / (.N)]

#作图
ggplot(data = chuli, mapping = aes(x = date, y =gzyz)) + geom_line()
ggplot(data = chuli, mapping = aes(x = date, y = )) + geom_line()
ggplot(chuli, aes(date))  + geom_line(aes(y = gzyz2, colour = "var0")) +     geom_line(aes(y = Clp, colour = "var1"))

write.csv(chuli,"C://Users//shenfan//Desktop//mydatam.csv")