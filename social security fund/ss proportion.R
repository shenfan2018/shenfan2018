#社保重仓
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)]

## 
social <- social[order(date, stock.id)
	][, .(sum.social = sum(proportion.current)), keyby = .(date, stock.id)]

social <- social[, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, year, quarter, sum.social)]

social <- social[order(stock.id,year,quarter)
	][, sum.social.2 := shift(sum.social, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, IO.social.change := sum.social - sum.social.2
	][order(year, quarter, - IO.social.change)
	][!is.na(IO.social.change)]

changes <- social[stock.q, on = .(stock.id, year, quarter), nomatch = 0
	][, changes := IO.social.change * 0.01 * 1000 * Dsmvosd
	][, social.change.tb := ntile(changes, 5), keyby = .(year, quarter)]


# 这里value-weighted按照这期的value
changes <- changes[, value := sum.social * 0.01 * 1000 * Dsmvosd
	][, proportion := value / (sum(value)), keyby = .(year, quarter, social.change.tb)
	][, .(ret.t0 = sum(ret.q.t * proportion), ret.t1 = sum(ret.q.t1 * proportion), ret.t12 = sum(ret.q.12 * proportion), ret.t13 = sum(ret.q.13 * proportion), ret.t14 = sum(ret.q.14 * proportion)), keyby = .(year, quarter, social.change.tb)]

# 表
Q5 <- changes[social.change.tb == 5]

mean(Q5[, ret.t0], na.rm = TRUE)

mean(Q5[, ret.t1], na.rm = TRUE)

mean(Q5[, ret.t12], na.rm = TRUE)

mean(Q5[, ret.t13], na.rm = TRUE)

mean(Q5[, ret.t14], na.rm = TRUE)

Q1 <- changes[social.change.tb == 1]

mean(Q1[, ret.t0], na.rm = TRUE)

mean(Q1[, ret.t1], na.rm = TRUE)

mean(Q1[, ret.t12], na.rm = TRUE)

mean(Q1[, ret.t13], na.rm = TRUE)

mean(Q1[, ret.t14], na.rm = TRUE)

t.test(Q5[, ret.t1], Q1[, ret.t1], paired = FALSE)