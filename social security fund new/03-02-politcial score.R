# politicial score

# holding
load("socialpf.RData")
social <- social[, proportion := MV.current / sum(MV.current, na.rm = TRUE), keyby = .(fund, year, quarter)]

# 往后滞后了一期，social的
social <- social[, .(fund, stock.id, date, year, quarter, proportion)
	][, year := ifelse(quarter == 4, year + 1, year)
	][, quarter := ifelse(quarter == 4, 1, quarter + 1)]

# SOE
SOE <- read_excel("C:/Users/shenfan/Desktop/社保/data/政治关联/中国上市公司股权性质文件/EN_EquityNatureAll.xlsx")
SOE <- as.data.table(SOE)
SOE <- SOE[, SOE := ifelse(EquityNatureID == 1, 1, 0)
	][, date := as.Date(EndDate)
	][, stock.id := Symbol
	][, year := year(date)
	][, .(stock.id, year, SOE)]

# match
data <- SOE[social, on = .(stock.id, year)
	][, SOE := ifelse(is.na(SOE), 0, 1)
	][, .(p_score = sum(SOE * proportion, na.rm = TRUE)), keyby = .(fund, date, year, quarter)
	][year < 2019
	]






# political connection
library(haven)
political <- read_stata("C:/Users/shenfan/Desktop/社保/data/政治关联/补充的政治关联数据.dta")
political <- as.data.table(political)
# CTB [董事会职务类别] - 1=董事长，2=副董事长，3=其他
# FGO_ServiSts[政治背景_任职状态] - 1 = 现任 ，2=曾任，98=无法确定任职状态，99=无政府背景。
political <- political[ctb == 1
	][!fgo_servists == 99 & fgo_servists == 98
	][, stock.id := as.character(stkcd)
	][, stock.id := str_pad(stock.id, 6, side = "left", pad = "0")
	][, polic := 1
	][, year := year(as.Date(reptdt))
	][, .(stock.id, year, polic)]

# match
data <- political[social, on = .(stock.id, year)
	][, polic := ifelse(is.na(polic), 0, 1)
	][, .(p_score = sum(polic * proportion, na.rm = TRUE)), keyby = .(fund, date, year, quarter)]

