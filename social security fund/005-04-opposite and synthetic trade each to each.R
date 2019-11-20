# 这里要注意好，不是marketvalue的change，而是holding的change而导致的marketvalue的change

# each to each 
# 这里注重change, proportion.change
# 公募基金持仓变化
fundpf <- read_excel("C:/Users/shenfan/Desktop/社保/data/公募基金持仓变化.xlsx")
fundpf <- as.data.table(fundpf)
setnames(fundpf, 1:7, c("code", "name", "report", "stock.id", "stock.name", "holding", "holding.change"))
setnames(fundpf, 9, c("current.mv"))
fundpf <- fundpf[, id := substring(code, 1, 6)
	][, stock.id := substring(stock.id, 1, 6)
	][, year := substring(report, 1, 4)
	][, report2 := substring(report, 6, 7)
	][, sem := ifelse(report2 == "中报", 1, 2)
	][, .(id, year, sem, stock.id, holding, holding.change, current.mv)]

# date
date.q <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date.q <- as.data.table(date.q)
date.q <- date.q[, date := as.Date(date)
	][, year := as.character(year(date))
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .SD[.N], keyby = .(year, sem)]
# match
fundpf <- date.q[fundpf, on = .(year, sem)
	][, .(id, date, stock.id, holding, holding.change, current.mv)]
setnames(fundpf, "holding.change", "fund_change")

# social security fund changes
# social security fund change to semi-annual
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/socialpf.RData")
social <- social[, .(fund, stock.id, date, hold.current, hold.change, MV.current)
	][, quarter := quarter(date)
	][, year := year(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, hold.change := sum(hold.change, na.rm = TRUE), keyby = .(fund, stock.id, year, sem)
	][quarter == 2 | quarter == 4
#	][, .SD[.N], keyby = .(fund, stock.id, year, sem)
	][, hold.change := as.numeric(ifelse(hold.change == 0, "NA", hold.change))
	][, .(fund, stock.id, date, hold.current, hold.change, MV.current)]
setnames(social, "hold.change", "social_change")

social.name <- social[order(fund)
	][, .(social.name = unique(fund))]
social.name <- social.name[["social.name"]]

# 
data <- list()
for (i in 1:26) {
	social.name1 <- social.name[i]

	data[[i]] <- fundpf[, fund := social.name1]
	data[[i]] <- social[data[[i]], on = .(fund, date, stock.id)
	][, price := MV.current / hold.current
	][, opposite := ifelse(fund_change * social_change < 0, 1, 0)
	][, change_all := (abs(fund_change) + abs(social_change)) * price
	][, change_min := min(abs(fund_change), abs(social_change)) * price, keyby = .(fund, id, date, stock.id)
	][opposite == 1
	][, .(opposite_num = (.N), opposite_sum = sum(change_all), opposite_min = min(change_min), opposite_dummy = 1), keyby = .(fund, id, date)
	][, fund := social.name1]
	rbindlist(data)
}

roll <- data.table(i = 1:26, data)
roll <- roll[1:26]

opposite.trade <- roll[, rbindlist(.SD[['data']]), by = i
	][, .(id, fund, date, opposite_num, opposite_sum, opposite_min, opposite_dummy)]

# add social_size
social_size <- social[, .(social_size = sum(MV.current, na.rm = TRUE), social_hold = (.N)), keyby = .(fund, date)]

# add fund_size
fund_size <- fundpf[, .(fund_size = sum(current.mv), fund_hold = (.N)), keyby = .(id, date)]

# match
opposite.trade <- social_size[opposite.trade, on = .(fund, date)]
opposite.trade <- fund_size[opposite.trade, on = .(id, date)]

# new 
opposite.trade <- opposite.trade[, opposite_num := opposite_num / (fund_hold + social_hold)
	][, opposite_sum := opposite_sum / (social_size + fund_size)
	][, opposite_min := opposite_min / (social_size + fund_size)
	][, .(id, fund, date, opposite_num, opposite_sum, opposite_min, opposite_dummy)]

save(opposite.trade, file = "oppositetrade.RData")

#################################################################
## Synthetic trading (仅改动hold.change * holding.change > 0, 1, 0))
# 这里要注意好，不是marketvalue的change，而是holding的change而导致的marketvalue的change

# each to each 
# 这里注重change, proportion.change
# 公募基金持仓变化
fundpf <- read_excel("C:/Users/shenfan/Desktop/社保/data/公募基金持仓变化.xlsx")
fundpf <- as.data.table(fundpf)
setnames(fundpf, 1:7, c("code", "name", "report", "stock.id", "stock.name", "holding", "holding.change"))
setnames(fundpf, 9, c("current.mv"))
fundpf <- fundpf[, id := substring(code, 1, 6)
	][, stock.id := substring(stock.id, 1, 6)
	][, year := substring(report, 1, 4)
	][, report2 := substring(report, 6, 7)
	][, sem := ifelse(report2 == "中报", 1, 2)
	][, .(id, year, sem, stock.id, holding, holding.change, current.mv)]

# date
date.q <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date.q <- as.data.table(date.q)
date.q <- date.q[, date := as.Date(date)
	][, year := as.character(year(date))
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .SD[.N], keyby = .(year, sem)]
# match
fundpf <- date.q[fundpf, on = .(year, sem)
	][, .(id, date, stock.id, holding, holding.change, current.mv)]
setnames(fundpf, "holding.change", "fund_change")

# social security fund changes
# social security fund change to semi-annual
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/socialpf.RData")
social <- social[, .(fund, stock.id, date, hold.current, hold.change, MV.current)
	][, quarter := quarter(date)
	][, year := year(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, hold.change := sum(hold.change, na.rm = TRUE), keyby = .(fund, stock.id, year, sem)
	][quarter == 2 | quarter == 4
	#	][, .SD[.N], keyby = .(fund, stock.id, year, sem)
	][, hold.change := as.numeric(ifelse(hold.change == 0, "NA", hold.change))
	][, .(fund, stock.id, date, hold.current, hold.change, MV.current)]
setnames(social, "hold.change", "social_change")

social.name <- social[order(fund)
	][, .(social.name = unique(fund))]
social.name <- social.name[["social.name"]]

# 
data <- list()
for (i in 1:26) {
	social.name1 <- social.name[i]

	data[[i]] <- fundpf[, fund := social.name1]
	data[[i]] <- social[data[[i]], on = .(fund, date, stock.id)
	][, price := MV.current / hold.current
	][, synthetic := ifelse(fund_change * social_change > 0, 1, 0)
	][, change_all := (abs(fund_change) + abs(social_change)) * price
	][, change_min := min(abs(fund_change), abs(social_change)) * price, keyby = .(fund, id, date, stock.id)
	][synthetic == 1
	][, .(synthetic_num = (.N), synthetic_sum = sum(change_all), synthetic_min = min(change_min), synthetic_dummy = 1), keyby = .(fund, id, date)
	][, fund := social.name1]
	rbindlist(data)
}

roll <- data.table(i = 1:26, data)
roll <- roll[1:26]

synthetic.trade <- roll[, rbindlist(.SD[['data']]), by = i
	][, .(id, fund, date, synthetic_num, synthetic_sum, synthetic_min, synthetic_dummy)]

# add social_size
social_size <- social[, .(social_size = sum(MV.current, na.rm = TRUE), social_hold = (.N)), keyby = .(fund, date)]

# add fund_size
fund_size <- fundpf[, .(fund_size = sum(current.mv), fund_hold = (.N)), keyby = .(id, date)]

# match
synthetic.trade <- social_size[synthetic.trade, on = .(fund, date)]
synthetic.trade <- fund_size[synthetic.trade, on = .(id, date)]

# new 
synthetic.trade <- synthetic.trade[, synthetic_num := synthetic_num / (fund_hold + social_hold)
	][, synthetic_sum := synthetic_sum / (social_size + fund_size)
	][, synthetic_min := synthetic_min / (social_size + fund_size)
	][, .(id, fund, date, synthetic_num, synthetic_sum, synthetic_min, synthetic_dummy)]

save(synthetic.trade, file = "synthetictrade.RData")

