# holding
load("IOS.RData")

# political connection
library(haven)
political <- read_stata("C:/Users/shenfan/Desktop/社保/data/政治关联/补充的政治关联数据.dta")
political <- as.data.table(political)
# CTB [董事会职务类别] - 1=董事长，2=副董事长，3=其他
# FGO_ServiSts[政治背景_任职状态] - 1 = 现任 ，2=曾任，98=无法确定任职状态，99=无政府背景。
political <- political[ctb == 1
	][!fgo_servists == 99 & fgo_servists == 98
	][, polic := 1
	][, year := year(as.Date(reptdt))
	][, stock.id := as.character(stkcd)
	][, stock.id := str_pad(stock.id, 6, side = "left", pad = "0")
	][, .(stock.id, year, polic)]

# match
data <- political[IOS.m, on = .(stock.id, year)
	][, polic := ifelse(is.na(polic), 0, 1)
	][, .(proportion = sum(proportion)), keyby = .(date, polic)]

