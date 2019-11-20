# all sample stocks on the basis of changes in IO
time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")

## 基金持股比例
IOF <- read_excel("C://Users//shenfan//Desktop//社保//data//基金持股比例.xlsx")
IOF <- as.data.table(IOF)
setnames(IOF, 1:58, c("code", "name", time))
IOF = melt(IOF, id.vars = c("code", "name"), measure.vars = time)
setnames(IOF, c("value", "variable"), c("IO.Fund", "DateQ"))

IOF <- IOF[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, quarter, IO.Fund)]

# 滞后
IOF <- IOF[, IO.Fund.2 := shift(IO.Fund, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, IO.Fund.change := IO.Fund - IO.Fund.2
	][order(year, quarter, -IO.Fund.change)
	][!is.na(IO.Fund.change)]

IOF <- IOF[, Fund.change.tb := ntile(IO.Fund.change, 5), keyby = .(year, quarter)]

load("stock.RData")
stock.q <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, ret.q.t := prod(Dretwd2, na.rm = TRUE) - 1, keyby = .(stock.id, year, month)
	][, .SD[.N], keyby = .(stock.id, year, month)]

# 滞后t+1
stock.q <- stock.q[, ret.q.t1 := shift(ret.q.t, n = 3, fill = NA, type = "lead"), keyby = .(stock.id)
	][, ret.q.t2 := shift(ret.q.t, n = 6, fill = NA, type = "lead"), keyby = .(stock.id)
	][, ret.q.t3 := shift(ret.q.t, n = 9, fill = NA, type = "lead"), keyby = .(stock.id)
	][, ret.q.t4 := shift(ret.q.t, n = 12, fill = NA, type = "lead"), keyby = .(stock.id)
	][, ret.q.12 := (1 + ret.q.t1) * (1 + ret.q.t2) - 1
	][, ret.q.13 := (1 + ret.q.12) * (1 + ret.q.t3) - 1
	][, ret.q.14 := (1 + ret.q.13) * (1 + ret.q.t4) - 1]

stock.q <- stock.q[, .(stock.id, year, month, quarter, Dsmvosd, ret.q.t, ret.q.t1, ret.q.12, ret.q.13, ret.q.14)]

changes <- IOF[stock.q, on = .(stock.id, year, quarter), nomatch = 0]

#去除季度
##changes <- IOF[stock.q, on = .(stock.id, year, quarter), nomatch = 0
##	][quarter == 2 | quarter == 4]

# 先是按照比例的分类,这里value-weighted按照上期的value
changes <- changes[, value := IO.Fund.2 * 0.01 * 1000 * Dsmvosd
	][, proportion := value / (sum(value)), keyby = .(year, month, Fund.change.tb)
	][, .(ret.t0 = sum(ret.q.t * proportion, na.rm = TRUE), ret.t1 = sum(ret.q.t1 * proportion, na.rm = TRUE), ret.t12 = sum(ret.q.12 * proportion, na.rm = TRUE), ret.t13 = sum(ret.q.13 * proportion, na.rm = TRUE), ret.t14 = sum(ret.q.14 * proportion, na.rm = TRUE)), keyby = .(year, month, Fund.change.tb)]

# 表
Q5 <- changes[Fund.change.tb == 5]

mean(Q5[, ret.t0], na.rm = TRUE)

mean(Q5[, ret.t1], na.rm = TRUE)

mean(Q5[, ret.t12], na.rm = TRUE)

mean(Q5[, ret.t13], na.rm = TRUE)

mean(Q5[, ret.t14], na.rm = TRUE)

Q1 <- changes[Fund.change.tb == 1]

mean(Q1[, ret.t0], na.rm = TRUE)

mean(Q1[, ret.t1], na.rm = TRUE)

mean(Q1[, ret.t12], na.rm = TRUE)

mean(Q1[, ret.t13], na.rm = TRUE)

mean(Q1[, ret.t14], na.rm = TRUE)

t.test(Q5[,ret.t0], Q1[,ret.t0], paired = TRUE)

#####社保
## 社保基金持股比例
IOS <- read_excel("C://Users//shenfan//Desktop//社保//data//社保基金持股比例.xlsx")
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IO.social", "DateQ"))

IOS <- IOS[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, year, quarter, IO.social)]

# 验算也并不是因为半年度还是季度的问题
##a <- IOS[!is.na(IO.social)
##	][, .(total = sum(IO.social)), keyby = .(year, quarter)]


# 滞后
IOS <- IOS[, IO.social.2 := shift(IO.social, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, IO.social.change := IO.social - IO.social.2
	][order(year, quarter, - IO.social.change)
	][!is.na(IO.social.change)]

IOS <- IOS[, social.change.tb := ntile(IO.social.change, 5), keyby = .(year, quarter)]

# value weighted
changes <- IOS[stock.q, on = .(stock.id, year, quarter), nomatch = 0]


# 先是按照比例的分类,这里value-weighted按照上期的value
changes <- changes[, value := IO.social.2 * 0.01 * 1000 * Dsmvosd
#	][, value.change := IO.social.change * 0.01 * 1000 * Dsmvosd
#	][, social.change.tb := ntile(value.change, 5), keyby = .(year, quarter)
	][, proportion := value / (sum(value)), keyby = .(year, month, social.change.tb)
	][, .(ret.t0 = sum(ret.q.t * proportion, na.rm = TRUE), ret.t1 = sum(ret.q.t1 * proportion, na.rm = TRUE), ret.t12 = sum(ret.q.12 * proportion, na.rm = TRUE), ret.t13 = sum(ret.q.13 * proportion, na.rm = TRUE), ret.t14 = sum(ret.q.14 * proportion, na.rm = TRUE)), keyby = .(year, month, social.change.tb)]


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

t.test(Q5[, ret.t1], Q1[, ret.t1], paired = TRUE)

