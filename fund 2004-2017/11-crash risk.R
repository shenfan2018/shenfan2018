# impact on stocks
# 俞老师的
crash_risk <- fread("C:/Users/shenfan/Desktop/基金经理//基金经理文献/俞老师/我的/crash_risk_all_new.csv", encoding = "UTF-8")

# 这是一种将1变成000001的方式
# a <- sprintf("%06d", 1:100)
crash.risk <- crash_risk[, stock.id := sprintf("%06d", Stkcd)
	][, sem := hcode - (year - 1991) * 2
	][, c("stock.id", "year", "sem", "NCSKEW", "DUVOL")]

data <- crash.risk

# 股票月换手率
turnover <- read_excel("C://Users//shenfan//Desktop//基金经理//data//2//crash risk//个股换手率表（月）//Liq_Tover_M.xlsx")
turnover <- as.data.table(turnover)
# 整理
turnover <- turnover[, c("Stkcd", "EndD", "ToverOsM")
	][Stkcd != "证券代码'", .SD
	][Stkcd != "没有单位'", .SD
	][, EndD := as.Date(EndD)
	][, Turnover.M := as.numeric(ToverOsM)
	][, stock.id := Stkcd
	][, year := year(EndD)
	][, quarter := quarter(EndD)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, Turnover.S := mean(Turnover.M, na.rm = TRUE)/100, keyby = .(stock.id, year, sem)
	][, yesno := duplicated(Turnover.S), keyby = .(stock.id, year, sem)
	][yesno == FALSE, .SD
	][, c("stock.id", "year", "sem", "Turnover.S")]

data <- turnover[data, on = .(stock.id, year, sem)]


# 股票月Amihud
amihud <- read_excel("C://Users//shenfan//Desktop//基金经理//data//2//crash risk//个股Amihud指标表（月）//Liq_Amihud_M.xlsx")
amihud <- as.data.table(amihud)
# 整理
amihud<- amihud[, c("Stkcd", "EndD", "ILLIQ_M")
	][Stkcd != "证券代码'", .SD
	][Stkcd != "没有单位'", .SD
	][, EndD := as.Date(EndD)
	][, Amihud.M := as.numeric(ILLIQ_M)
	][, stock.id := Stkcd
	][, year := year(EndD)
	][, quarter := quarter(EndD)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, Amihud.S := mean(Amihud.M, na.rm = TRUE), keyby = .(stock.id, year, sem)
	][, yesno := duplicated(Amihud.S), keyby = .(stock.id, year, sem)
	][yesno == FALSE, .SD
	][, c("stock.id", "year", "sem", "Amihud.S")]

data <- amihud[data, on = .(stock.id, year, sem)]

# 周的stock return
load("stock.RData")
#整理
stock <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, date := as.Date(date)]

stock <- stock[order(stock.id, date)
	][, week := isoweek(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2]

# 变成week
stock.w <- stock
stock.w <- stock.w[, Dretwd2 := 1 + Dretwd
	][, return.w := prod(Dretwd2, na.rm = TRUE) - 1, keyby = .(stock.id, year, week)
	][, yesno := duplicated(return.w), keyby = .(stock.id, year, week)
	][yesno == FALSE, .SD
	][, std_stock := sd(return.w, na.rm = TRUE), keyby = .(stock.id, year, sem)
	][, return_mean := mean(return.w, na.rm = TRUE), keyby = .(stock.id, year, sem)
	][, yesno := duplicated(std_stock), keyby = .(stock.id, year, sem)
	][yesno == FALSE, .SD
	][, c("stock.id", "year", "sem", "return_mean", "std_stock")]

data <- stock.w[data, on = .(stock.id, year, sem)]


# size，这里选择的个股总市值
load("stock.RData")
#整理
stock.size <- stock[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Dsmvtll := as.numeric(as.character(Dsmvtll))
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][order(stock.id, date)
	][, size := Dsmvtll[.N]*1000, keyby = .(stock.id, year, sem)
	][, logsize := log(size)
	][, yesno := duplicated(size), keyby = .(stock.id, year, sem)
	][yesno == FALSE, .SD
	][, c("stock.id", "year", "sem", "logsize","size")]


# MB，股东权益合计/总市值
MB <- read_excel("C://Users//shenfan//Desktop//基金经理//data//2//crash risk//股东权益合计.xlsx")
MB <- as.data.table(MB)
setnames(MB, 1:38, c("code", "name", "2000-06-30", "2000-12-31", "2001-06-30", "2001-12-31", "2002-06-30", "2002-12-31", "2003-06-30", "2003-12-31", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
MB = melt(MB, id.vars = c("code", "name"), measure.vars = c("2000-06-30", "2000-12-31", "2001-06-30", "2001-12-31", "2002-06-30", "2002-12-31", "2003-06-30", "2003-12-31", "2004-06-30", "2004-12-31", "2005-06-30", "2005-12-31", "2006-06-30", "2006-12-31", "2007-06-30", "2007-12-31", "2008-06-30", "2008-12-31", "2009-06-30", "2009-12-31", "2010-06-30", "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30", "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(MB, c("value", "variable"), c("equity", "date"))
MB <- MB[, date := as.Date(as.character(date))
	][, name := NULL
	][, stock.id := substring(code, 1, 6)
	][order(code, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 2, sem := 1
	][quarter == 4, sem := 2]

# 计算MB
MB <- stock.size[MB, on = .(stock.id, year, sem), nomatch = 0]

MB <- MB[, Market_to_value := equity / size
	][, c("stock.id", "date", "year", "sem", "logsize", "Market_to_value")]

#这里的MB是2003-2017年的，所有A股了
data <- MB[data, on = .(stock.id, year, sem), nomatch = 0]

crash.risk <- data

# 其实最后一个就是dummy_invest了
# low low的基金的股票
load("fund9.RData")
data <- fund.9
data <- data[order(year, sem, category2, - sem_return.1)
	][, rank.1 := sequence(.N), by = .(year, sem, category2)]

# 这里啊 要把NA的去掉哦
data <- data[, na := ifelse(is.na(sem_return.1), 1, 0)]
# 这里把sem_return.1缺失的都删掉了
data <- data[na == 0, .SD
	][, per.tb := ntile(rank.1, 5), keyby = .(year, sem)]
data <- data[per.tb == 1, flow.tb := ntile(netflow.1, 5)
	][per.tb == 2, flow.tb := ntile(netflow.1, 5)
	][per.tb == 3, flow.tb := ntile(netflow.1, 5)
	][per.tb == 4, flow.tb := ntile(netflow.1, 5)
	][per.tb == 5, flow.tb := ntile(netflow.1, 5)]

lowlow <- data[per.tb == 5 & flow.tb == 1, .SD
	][, c("id", "year", "sem")]

# 找到portfolio
load("fund-portfolio.RData")
portfolio <- portfolio[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 2, sem := 1
	][quarter == 4, sem := 2
	][, c("id", "year", "sem", "stock.id")]

portfolio <- lowlow[portfolio, on = .(id, year, sem), nomatch = 0]

stock.p <- portfolio
stock.p <- stock.p[order(year, sem, stock.id)
	][, invest.num := .N, keyby = .(stock.id, year, sem)
	][, yesno := duplicated(stock.id), keyby = .(year, sem)
	][yesno == FALSE, .SD
	][, invest := 1
	][, c("stock.id", "year", "sem", "invest","invest.num")]

crash.risk <- stock.p[crash.risk, on = .(stock.id, year, sem)]

crash.risk <- crash.risk[, invest := ifelse(is.na(invest), 0, 1)
	][invest == 0, invest.num := 0]

save(crash.risk, file = "crash-risk.RData")

# 数据的一些差分和回归
load("crash-risk.RData")
crash.risk <- crash.risk[order(stock.id, year, sem)
	][, NCSKEW.d1 := shift(NCSKEW, n = 1, fill = NA, type = "lead"), keyby = stock.id
	][, DUVOL.d1 := shift(DUVOL, n = 1, fill = NA, type = "lead"), keyby = stock.id
	][, delta_turnover := c(NA, diff(Turnover.S, difference = 1)), keyby = stock.id
	][, delta_amihud := c(NA, diff(Amihud.S, difference = 1)), keyby = stock.id]

write.csv(crash.risk, "C://Users//shenfan//Desktop//crashrisk.csv")


liner1 <- plm(NCSKEW.d1 ~ invest + NCSKEW + delta_turnover + return_mean + logsize + Market_to_value + std_stock, crash.risk, model = "within", effect = "twoways", index = c("stock.id", "date"))

liner2 <- plm(DUVOL.d1 ~ invest + DUVOL + delta_turnover + return_mean + logsize + Market_to_value + std_stock, crash.risk, model = "within", effect = "twoways", index = c("stock.id", "date"))

liner3 <- plm(NCSKEW.d1 ~ invest + NCSKEW + delta_amihud + return_mean + logsize + Market_to_value + std_stock, crash.risk, model = "within", effect = "twoways", index = c("stock.id", "date"))

liner4 <- plm(DUVOL.d1 ~ invest + DUVOL + delta_amihud + return_mean + logsize + Market_to_value + std_stock, crash.risk, model = "within", effect = "twoways", index = c("stock.id", "date"))

liner5 <- plm(NCSKEW.d1 ~ invest.num + NCSKEW + delta_turnover + return_mean + logsize + Market_to_value + std_stock, crash.risk, model = "within", effect = "twoways", index = c("stock.id", "date"))

liner6 <- plm(DUVOL.d1 ~ invest.num + DUVOL + delta_turnover + return_mean + logsize + Market_to_value + std_stock, crash.risk, model = "within", effect = "twoways", index = c("stock.id", "date"))

liner7 <- plm(NCSKEW.d1 ~ invest.num + NCSKEW + delta_amihud + return_mean + logsize + Market_to_value + std_stock, crash.risk, model = "within", effect = "twoways", index = c("stock.id", "date"))

liner8 <- plm(DUVOL.d1 ~ invest.num + DUVOL + delta_amihud + return_mean + logsize + Market_to_value + std_stock, crash.risk, model = "within", effect = "twoways", index = c("stock.id", "date"))


stargazer(liner1, liner2, liner3, liner4, liner5, liner6, liner7, liner8, type = "html", out = "C://Users//shenfan//Desktop//基金经理//data//2//crash risk//crashrisk222.doc", report = ('vc*t'), add.lines = list(c("stock", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")))

