## 
overlap.hl <- overlap

overlap.hl <- overlap.hl[order(year, sem, id)
	][, overlap1.ntile := ntile(overlap1, 5), keyby = .(year, sem)
	][, overlap2.ntile := ntile(overlap2, 5), keyby = .(year, sem)
	][order(id, year, sem)
	][, .(id, year, sem, overlap1.ntile)]

# 所有基金名称
load("portfolio.RData")
mutual <- portfolio[order(id)
	][, .(id)
	][, .(id = unique(id))]
mutual <- mutual[["id"]]

date <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间.xlsx")
date <- date[["date"]]

#perfect 三列相拼
data <- CJ(fund.1 = mutual, fund.2 = mutual, date = date)
data <- data[, year := year(date)
	][, month := month(date)
	][year < 2018]

#基金的monthly return
load("fund-NAV.RData")
fund <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(return.f = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)]
setnames(fund, "id", "fund.1")
data <- fund[data, on = .(fund.1, year, month)]
setnames(fund, "fund.1", "fund.2")
data <- fund[data, on = .(fund.2, year, month)]

data <- data[month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, sem := 1
	][month == 7 | month == 8 | month == 9 | month == 10 | month == 11 | month == 12, sem := 2]

high <- overlap.hl[, overlap1.ntile.1 := shift(overlap1.ntile, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][overlap1.ntile.1 == 5
	][, .(id, year, sem, overlap1.ntile.1)]
setnames(high, "id", "fund.1")

data <- high[data, on = .(fund.1, year, sem), nomatch = 0]

low <- overlap.hl[, overlap1.ntile.1 := shift(overlap1.ntile, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][overlap1.ntile.1 != 5
	][, .(id, year, sem, overlap1.ntile.1)]
setnames(low, "id", "fund.2")
data <- low[data, on = .(fund.2, year, sem), nomatch = 0]

# fund family
fund.co <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人.xlsx")
fund.co <- as.data.table(fund.co)
setnames(fund.co, 1:3, c("code", "name", "fund.company"))
fund.co <- fund.co[, id := substring(code, 1, 6)
	][, c("id", "fund.company")]
setnames(fund.co, "id", "fund.1")
data <- fund.co[data, on = .(fund.1)]
setnames(fund.co, "fund.1", "fund.2")
data <- fund.co[data, on = .(fund.2)]

data <- data[, same := ifelse(fund.company == i.fund.company, 1, 0)]
# high-low
data <- data[, dif := i.return.f - return.f]

plm(dif ~ same, data, model = "within", effect = "twoways", index = c("fund.1", "date")) %>% summary()

data[, felm(dif ~ same | fund.1 + year)] %>% summary()