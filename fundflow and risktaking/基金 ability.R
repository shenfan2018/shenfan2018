#自己算的权重(这里选取的是含限售股的市值）
stock.mv <- read_excel("C://Users//shenfan//Desktop//data//指数成分股权重//A market value.xlsx")
stock.mv <- as.data.table(stock.mv)
setnames(stock.mv, 1:18, c("code", "name", "2010-06-30",  "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30",  "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
stock.mv = melt(stock.mv, id.vars = c("code", "name"), measure.vars = c("2010-06-30",  "2010-12-31", "2011-06-30", "2011-12-31", "2012-06-30",  "2012-12-31", "2013-06-30", "2013-12-31", "2014-06-30", "2014-12-31", "2015-06-30", "2015-12-31", "2016-06-30", "2016-12-31", "2017-06-30", "2017-12-31"))
setnames(stock.mv, "variable", "Date")
setnames(stock.mv, 4, c("stock_mv"))
stock.mv <- stock.mv[, Date := as.Date(as.character(Date))
	][code != "NA"
	][code != "数据来源：Wind", .SD
	][, stock.id := substring(code, 1, 6)]

stock.mv <- stock.mv[, proportion := 100 * stock_mv / sum(stock_mv, na.rm = TRUE), keyby = Date
	][, year := year(Date)
	][, quarter := quarter(Date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][,c("stock.id","year","sem","proportion")]


# 下载的权重
# 试了一下不管了 还是用自己算的了
# xzqz <- fread("C://Users//shenfan//Desktop//data//指数成分股权重//szzhengli.csv")
# xzqz <- xzqz[, Date := as.Date(Date)
#	][, id := as.character(id)
#	][,V1:=NULL]


## stock.sem 为Rit+1
#读取stock数据，每日的，包括收盘价和收益率，命名为stock
load("datastock.RData")

stock.sem <- stock
# sem.return是下半年的收益率
stock.sem <- stock.sem[order(stock.id, date)
	][, Dretwd2 := 1 + Dretwd
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, sem.return := prod(Dretwd2, na.rm = FALSE) - 1, keyby = .(stock.id, year, sem)]
# 变成半年度数据
stock.sem <- stock.sem[, cf := (duplicated(sem.return, keyby = .(stock.id, year, sem)))
	][cf == FALSE, .SD
	][, c("date", "Clsprc", "Dretwd", "Dretwd2", "quarter", "cf") := NULL]
# 向前一格,即sem.return.1为Rit+1
stock.sem <- stock.sem[, sem.return.1 := shift(sem.return, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)
	][year != 2018, .SD]



## Rmt+1 名字market.return(暂时ok了）
# 下面计算Rmt+1,这里还是用capm的吧，综合市场收益
market.return <- read_excel("C://Users//shenfan//Desktop//data//CAPM//综合日市场回报率文件2010-2018.6//TRD_Cndalym.xlsx")
market.return <- as.data.table(market.return)
#选择type
market.return <- market.return[Markettype == "5"][, Markettype := NULL]
setnames(market.return, 1:2, c("Date","market_return")) #考虑现金红利的综合日市场回报率(流通市值加权平均法)'

market.return <- market.return[Date != "没有单位'", .SD
	][Date != "交易日期'", .SD
	][, Date := as.Date(Date)
	][, market_return := as.numeric(as.character(market_return))
	][order(Date)
	][, year := year(Date)
	][, quarter := quarter(Date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, market_return2 := market_return + 1
	][, Rmarket := prod(market_return2, na.rm = FALSE) - 1, keyby = .(year, sem)]
# 变成半年度数据
market.return <- market.return[, cf := (duplicated(Rmarket, keyby = .(year, sem)))
	][cf == FALSE, .SD
	][, c("year", "sem", "Rmarket")]
# 向前一格,即Rmarket.1为RMt+1
market.return <- market.return[, Rmarket.1 := shift(Rmarket, n = 1, fill = NA, type = "lead")]



## 计算betait
# 计算beta CAPM r-rs=a+b*(rm-rs)
#以stock为主表
load("datastock.RData")
setnames(stock,"date","Date")
data.c <- stock
#匹配market.return
#加入riskfree
risk.free <- read_excel("C://Users//shenfan//Desktop//data//CAPM//risk-free.xlsx")
risk.free <- as.data.table(risk.free)
setnames(risk.free, 1:2,c("Date","risk_free"))
risk.free <- risk.free[, Date := as.Date(Date)
	][, risk_free.daily := (risk_free + 1) ^ (1 / 365) - 1]
#riskfree匹配上主表
data.c <- risk.free[data.c, on = .(Date), nomatch = 0]


#market
market <- read_excel("C://Users//shenfan//Desktop//data//CAPM//综合日市场回报率文件all//TRD_Cndalym.xlsx")
market <- as.data.table(market)
#选择type
market <- market[Markettype == "5"][, Markettype := NULL]

setnames(market, 1:2, c("Date","market_return")) #考虑现金红利的综合日市场回报率(流通市值加权平均法)'
market <- market[Date != "没有单位'", .SD][Date != "交易日期'", .SD
	][, Date := as.Date(Date)
	][, market_return := as.numeric(as.character(market_return))]

#匹配market risk
data.c <- market[data.c, on = .(Date), nomatch = 0]


## 数据在data.c 
data.c <- data.c[, return := Dretwd - risk_free.daily
	][, preimum := market_return - risk_free.daily
	][order(stock.id, Date)]

# 虽然有点蠢
data.date <- read_excel("C://Users//shenfan//Desktop//data//时间的.xlsx")
data.date <- as.data.table(data.date)
setnames(data.date, "Date", "DateQ")
data.date <- data.date[, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][quarter == 2 | quarter == 4, tag := sequence(.N)
	][quarter == 1 | quarter == 3, tag := sequence(.N)]

#匹配具体时间
data <- data.c
data <- data[, year := year(Date)
	][, quarter := quarter(Date)]

data <- data.date[data, on = .(year, quarter)]


#tag reg.roll是一个list里面套data.table
reg.roll <- list()
for (i in 2:17) {
	reg.roll[[i]] <- data[tag >= i - 1 & tag <= i, {
		I <- lm(return ~ preimum) %>% coef() %>% as.list()
	},
	keyby = .(stock.id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:17, reg.roll)
roll <- roll[2:17]


reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
setnames(reg.cof, "preimum", "beta")
reg.cof<-reg.cof[,c("(Intercept)"):=NULL]
#这里就完成了
data <- reg.cof[data, on = .(stock.id, tag), nomatch = 0]

#日度变成半年度
data <- data[, cf := (duplicated(beta, keyby = .(stock.id,tag)))
	][cf == FALSE, .SD
	][, c("stock.id","beta","year","sem")]

#保存beta们 半年度的 按照日度 所有A股的beta
save(data, file = "databeta.RData")


##整理一下 
# 计算了Rit+1 stock.sem
# 计算了Rmt+1 market.return
# 就算了betait data.beta
load("databeta.RData")
data.beta <- data
# 计算的个股占A股市场的比重 stock.mv

#将他们合并 先以股票为主的整理
data <- data.beta #加入beta
data <- market.return[data, on = .(sem, year), nomatch = 0] #加入Rmt+q
data <- stock.sem[data, on = .(stock.id, sem, year), nomatch = 0] #加入rit+1
data <- stock.mv[data, on = .(stock.id, sem, year), nomatch = 0] #加入wimt

data.ability <- data
save(data.ability, file = "dataability.RData")



# 下面开始,以portfolio为主，dataability作为补充（这里运行）
load("fund-portfolio.RData")
load("dataability.RData")
data <- portfolio
data <- data[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2]

data <- data.ability[data, on = .(stock.id, year, sem),nomatch=0]  #nomatch=0以后 将会剔除部分基金持有的新股，入001725这只基金，2017年6月30半年报就持有002879这只股票，但是这股票实际上2017-7-7才上市，beta无，proportion无。


# 计算择时和选股能力,fund gap,return.org按照原本的持股的收益
data <- data[, timing.single := (proportion.stock - proportion) * (beta * Rmarket.1)
	][, timing := sum(timing.single, na.rm = TRUE), keyby = .(id, year, sem)
	][, picking.single := (proportion.stock - proportion) * (sem.return.1 - beta * Rmarket.1)
	][, picking := sum(picking.single, na.rm = TRUE), keyby = .(id, year, sem)
	][, return.org.single := proportion.stock * sem.return.1
	][, return.org := sum(return.org.single, na.rm = TRUE), keyby = .(id, year, sem)]

#变半年度,现在都是t期的，要t+1期
data <- data[, cf := (duplicated(timing, keyby = .(id, year,sem)))
	][cf == FALSE, .SD
	][, c("id", "name", "year", "sem", "fund.category", "date", "timing", "picking", "return.org")]

# data这里的数据都是t+1的，所以要滞后过去，即把date往前提一格
data <- data[, Date := shift(date, n = 1, fill = NA, type = "lead"), by = id
	][date == "2017-06-30", Date := as.Date("2017-12-31")
	][, c("date", "year", "sem") := NULL
	][, year := year(Date)
	][, quarter := quarter(Date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2]


## 计算fund gap 还需要一个基金的年度收益率，名称data.NAV
load("fund-NAV.RData")
setnames(data.NAV, "date", "Date")
data.NAV <- data.NAV[order(Date)
	][, year := year(Date)
	][, quarter := quarter(Date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, AdjustedNAVGrowth2 := AdjustedNAVGrowth + 1
	][, RNAV := prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1, keyby = .(id, year, sem)
	][, cf := (duplicated(RNAV, keyby = .(id, year, sem)))
	][cf == FALSE, .SD
	][, c("id", "year", "sem", "RNAV")]



# 将fund-NAV并入
data <- data.NAV[data, on = .(id, year, sem), nomatch = 0]


data <- data[, fund.gap := RNAV - return.org
	][,c("quarter"):=NULL]


fund.ability <- data
save(fund.ability, file = "fund-ability.RData")


# 再次加入churn rate基金的换手率
load("datachurn.RData")
load("fund-ability.RData")
data.churn <- data.churn[, c("id", "DateQ", "churn.rate")]
setnames(fund.ability, "Date", "DateQ")
fund.ability <- data.churn[fund.ability, on = .(id, DateQ)]


# 并入主表
data <- fund.ability
data <- data[, c("id", "DateQ", "churn.rate", "timing", "picking", "fund.gap")]

load("datamain5.RData")
data <- data[data.main5, on = .(id, DateQ)]

summary(plm(r.total.c ~ netflow.1 * timing + netflow.1 * picking + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))

