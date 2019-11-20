# 新增吧，主表是data.main5以及fund.ability
load("datamain5.RData")
load("fund-ability.RData")
load("datachurn.RData")


data <- data.main5
setnames(fund.ability, "Date", "DateQ")
fund.ability <- fund.ability[, c("id", "DateQ", "timing", "picking", "fund.gap")]
data <- fund.ability[data, on = .(id, DateQ)]

data.churn <- data.churn[, c("id", "DateQ", "churn.rate")]
data<-data.churn[data,on=.(id,DateQ)]

data <- data[order(id, DateQ)]

data <- data[, churn.rate.2 := shift(churn.rate, n = 1, fill = NA, type = "lead"), by = .(id, year)
	][, timing.2 := shift(timing, n = 1, fill = NA, type = "lead"), by = .(id, year)
	][, picking.2 := shift(picking, n = 1, fill = NA, type = "lead"), by = .(id, year)
	][, fund.gap.2 := shift(fund.gap, n = 1, fill = NA, type = "lead"), by = .(id, year)
	][quarter == 3 | quarter == 1, churn.rate := churn.rate.2
	][quarter == 3 | quarter == 1, timing := timing.2
	][quarter == 3 | quarter == 1, picking := picking.2
	][quarter == 3 | quarter == 1, fund.gap := fund.gap.2
	][, c("churn.rate.2", "timing.2", "picking.2", "fund.gap.2") := NULL]


#data.fi终，不过问题多多，不要深追啊，可怕可怕
data.fi <- data


# 加入rank.1
data <- data[order(DateQ, -period_return.1)
	][, rank.1 := sequence(.N), keyby = .(DateQ, category2)]
