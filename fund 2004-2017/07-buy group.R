# 试试buy group
load("fund-portfolio.RData")

data <- portfolio
data <- data[order(date, stock.id)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, stock.buy := .N, keyby = .(year, sem, stock.id)]

# 获得半年度数据
dataQ <- data
dataQ <- dataQ[, yesno := duplicated(stock.buy), keyby = .(stock.id, year, sem)
	][yesno == FALSE, .SD
	][, c("stock.id", "year", "sem", "stock.buy")]

dataQ <- dataQ[order(year, sem, - stock.buy)
	][, tenth := quantile(stock.buy, 0.9), keyby = .(year, sem)
	][, group := ifelse(stock.buy > tenth, 1, 0)
	][, c("stock.id", "year", "sem", "group")]

# 注意这里的dataQ是当期的，也就是portfolio有没有买上期的group，dataQ要滞后
dataQ <- dataQ[order(stock.id, year, sem)
	][, group.1 := shift(group, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][year > 2003, .SD]


# 到portfolio
load("fund-portfolio.RData")
group <- portfolio
group <- group[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2]
group <- dataQ[group, on = .(stock.id, year, sem)]

# 计算group的比例
group <- group[order(id, year, sem)
	][, simple.group := sum(group.1, na.rm = TRUE) / .N, keyby = .(id, year, sem)
	][, cj := group.1 * proportion.stock
	][, wa.group := sum(cj, na.rm = TRUE), keyby = .(id, year, sem)]

# 变成半年度,与前beta一致，8588数据
group <- group[, yesno := duplicated(wa.group), keyby = .(id, year, sem)
	][yesno == FALSE, .SD
	][, c("id", "year", "sem", "simple.group", "wa.group")]

load("fund6.RData")
fund.7 <- group[fund.6, on = .(id, year, sem)]

save(fund.7, file = "fund7.RData")