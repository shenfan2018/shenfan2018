# each to each 
load("socialpf.RData")
social <- social[order(fund, year, sem)
	][, .(fund, fund.company, date, stock.id)
	][, buy := 1]

social.name <- social[order(fund)
	][, .(social.name = unique(fund))]
social.name <- social.name[["social.name"]]

####
# choose one of these two
# portfolio csmar
load("portfolio.RData")
portfolio <- portfolio[, .(id, name, date, stock.id, MarketValue, proportion.stock)
	][order(id, date, stock.id)]

# portfolio wind
fundpf <- read_excel("C:/Users/shenfan/Desktop/社保/data/公募基金持仓变化.xlsx")
fundpf <- as.data.table(fundpf)
setnames(fundpf, 1:7, c("code", "name", "report", "stock.id", "stock.name", "holding", "holding.change"))
setnames(fundpf, 9, c("current.mv"))
fundpf <- fundpf[, id := substring(code, 1, 6)
	][, stock.id := substring(stock.id, 1, 6)
	][, year := substring(report, 1, 4)
	][, report2 := substring(report, 6, 7)
	][, sem := ifelse(report2 == "中报", 1, 2)
	][, .(id, year, sem, stock.id, holding, holding.change, current.mv)
	][, proportion.stock := current.mv / sum(current.mv), keyby = .(id, year, sem)
	][order(id, year, sem, stock.id)]
date.q <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date.q <- as.data.table(date.q)
date.q <- date.q[, date := as.Date(date)
	][, year := as.character(year(date))
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .SD[.N], keyby = .(year, sem)]
# match
portfolio <- date.q[fundpf, on = .(year, sem)
	][, .(id, date, stock.id, proportion.stock)]


data <- list()
for (i in 1:26) {
	social.name1 <- social.name[i]

	data[[i]] <- portfolio[, fund := social.name1]
	data[[i]] <- social[data[[i]], on = .(fund, date, stock.id)
	][, buy := ifelse(is.na(buy), 0, 1)
	][, .(overlap1 = sum(buy) / (.N), overlap2 = sum(proportion.stock * buy) / sum(proportion.stock)), keyby = .(id, date)
	][, fund := social.name1]
	rbindlist(data)
}

roll <- data.table(i = 1:26, data)
roll <- roll[1:26]

overlap <- roll[, rbindlist(.SD[['data']]), by = i
	][, .(id, fund, date, overlap1, overlap2)]

save(overlap, file = 'overlap.RData')

### 基本没差多少！ 干！
