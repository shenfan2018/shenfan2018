#社保重仓
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 2 | quarter == 4
	][, sem := ifelse(quarter == 2, 1, 2)]

social <- social[, proportion := MV.current / sum(MV.current, na.rm = TRUE), keyby = .(fund, year, sem)]

########################################################### 
### 新版本 single social holding return sem-annual
load("socialpf.RData")
social <- social[, proportion := MV.current / sum(MV.current, na.rm = TRUE), keyby = .(fund, year, sem)]

# stock ,这里滞后了，若要同期，则删除最后两行
load("stock.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_ret = prod(Dretwd2, na.rm = TRUE) - 1), keyby = .(stock.id, year, sem, month)]

# 往后滞后了一期，social的
social <- social[, .(fund, stock.id, date, year, sem, proportion)
	][, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]

# 目前ret是 期的持仓算t期的收益
social.ret <- stock.m[social, on = .(stock.id, year, sem)
	][, .(ret = sum(proportion * month_ret, na.rm = TRUE)), keyby = .(fund, year, month)
	][year < 2018
	][!is.na(month)]

save(social.ret, file = "social_ret.RData")

load("social_ret.RData")
a <- social.ret[, ret2 := ret + 1
	][, .(c.ret = prod(ret2) - 1, N = (.N) / 12), keyby = .(fund)
	][, annual.ret := (c.ret + 1) ^ (1 / N) - 1]


