# big data
# Amihud 2003.1.1-2018.12.31
liq0 <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/个股Amihud指标表（日）/Liq_Amihud_D.xlsx")
liq1 <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/个股Amihud指标表（日）/Liq_Amihud_D1.xlsx")
liq2 <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/个股Amihud指标表（日）/Liq_Amihud_D2.xlsx")
liq3 <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/个股Amihud指标表（日）/Liq_Amihud_D3.xlsx")
liq4 <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/个股Amihud指标表（日）/Liq_Amihud_D4.xlsx")
liq5 <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/个股Amihud指标表（日）/Liq_Amihud_D5.xlsx")
liq6 <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/个股Amihud指标表（日）/Liq_Amihud_D6.xlsx")
liq7 <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/个股Amihud指标表（日）/Liq_Amihud_D7.xlsx")
liq8 <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/个股Amihud指标表（日）/Liq_Amihud_D8.xlsx")
liq.raw <- rbindlist(list(liq0, liq1, liq2, liq3, liq4, liq5, liq6, liq7, liq8)) #stock原文件 勿删
rm(liq0, liq1, liq2, liq3, liq4, liq5, liq6, liq7, liq8)
liq <- liq.raw
liq <- as.data.table(liq)
setnames(liq, c("Trddt", "Stkcd"), c("date", "stock.id"))
liq <- liq[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][MarketType == 4 | MarketType == 1
	][, ILLIQ := as.numeric(ILLIQ)
	][, date := as.Date(date)
	][order(stock.id, date)]
liq <- liq[, c("stock.id", "date", "ILLIQ")]

# 这里有过save了
save(liq, file = "liq.RData")
