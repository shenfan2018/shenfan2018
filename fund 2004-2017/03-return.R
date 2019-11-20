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

load("fund-NAV.RData")
# 半年的收益率
data.NAV <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, sem_return := prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1, keyby = .(id, year, sem)
	][, fund.risk := sd(AdjustedNAVGrowth, na.rm = TRUE), keyby = .(id, year, sem)
	][, mean := mean(AdjustedNAVGrowth, na.rm = TRUE), keyby = .(id, year, sem)
	][, fund.sk := sum((AdjustedNAVGrowth - mean) ^ 3 / fund.risk ^ 3) / (.N), keyby = .(id, year, sem)]

#变成半年度数据
data.NAV <- data.NAV[, yesno := duplicated(sem_return), keyby =.(id, year, sem)
	][yesno == FALSE, .SD
	][, c("id", "year", "sem", "sem_return", "fund.risk", "fund.sk")]


load("fund1.RData")
fund.2 <- data.NAV[fund.1, on = .(id, year, sem), nomatch = 0]

save(fund.2, file = "fund2.RData")
