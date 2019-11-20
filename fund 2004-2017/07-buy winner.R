# buy winner
load("stock.RData")
#整理
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, date := as.Date(date)]

stock <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, Dretwd2 := 1 + Dretwd
	][, sem_return := prod(Dretwd2, na.rm = TRUE) - 1, keyby = .(stock.id, year, sem)]

# 得到半年度数据
stockQ <- stock
stockQ <- stockQ[, yesno := duplicated(sem_return), keyby = .(stock.id, year, sem)
	][yesno == FALSE, .SD
	][, c("stock.id", "year", "sem", "sem_return")]

stockQ <- stockQ[order(year,sem, - sem_return)
	][, tenth := quantile(sem_return, 0.9), keyby = .(year, sem)
	][, winner := ifelse(sem_return > tenth, 1, 0)
	][, c("stock.id", "year", "sem", "winner")]

# 注意这里的stockQ是当期的，也就是portfolio有没有买上期的winner，stockQ要滞后
stockQ <- stockQ[order(stock.id, year, sem)
	][, winner.1 := shift(winner, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][year >2003, .SD]


# 到portfolio
load("fund-portfolio.RData")
winner<-portfolio
winner <- winner[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2]
winner <- stockQ[winner, on = .(stock.id, year, sem)]

# 计算winner的比例
winner <- winner[order(id, year, sem)
	][, simple.winner := sum(winner.1,na.rm = TRUE) / .N, keyby = .(id, year, sem)
	][, cj := winner.1 * proportion.stock
	][, wa.winner := sum(cj, na.rm = TRUE), keyby = .(id, year, sem)]

# 变成半年度,与前beta一致，8588数据
winner <- winner[, yesno :=duplicated(wa.winner),by = c("id", "year", "sem")
	][yesno == FALSE, .SD
	][, c("id", "year", "sem", "simple.winner", "wa.winner")]

load("fund4.RData")
# fund.4 加入winner
fund.4 <- winner[fund.4, on = .(id, year, sem)]

# fund.4加入churn.rate
load("datachurn.RData") #data.churn
data.churn <- data.churn[quarter == 2, sem := 1
	][quarter == 4, sem := 2
	][, c("code", "year", "sem", "turnover.rate", "churn.rate")]
fund.4 <- data.churn[fund.4, on = .(code, year, sem)]

fund.5 <- fund.4
save(fund.5, file = "fund5.RData")

