# 最后一个回归
# 先加入是否量化
quant <- fread("C://Users//shenfan//Desktop//基金经理文献//俞老师//我的//Fund_Strategy.csv")
setnames(quant, c("量化", "策略", "模型"), c("quant", "strategy", "model"))
quant <- quant[, id := sprintf("%06d", MasterFundCode)
	][, .(id, quant, strategy, model)]

load("fund9.RData")
data.quant <- quant[fund.9, on = .(id)
	][is.na(quant), quant := 0
	][year > 2014, .SD]

data.quant <- data.quant[, yesno := duplicated(alpha_3), keyby = .(id, year, sem)
	][yesno == FALSE, .SD]

# 这里改成升序了
data.quant <- data.quant[order(year, sem, category2, sem_return.1)
	][, rank.1 := sequence(.N), keyby = .(year, sem, category2)
	][, returnna := ifelse(is.na(sem_return.1), 1, 0)
	][returnna == 1, rank.1 := sem_return.1]

# risk taking change driven by net flow，数值太小了
data.quant <- data.quant[, fund.risk.c := fund.risk.c * 100
	][, fund.sk.c := fund.sk.c * 100]

liner5 <- plm(fund.risk.c ~ netflow.1 * strategy + rank.1 + logfund_size.1 + logfund_age.1, data.quant, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner6 <- plm(fund.sk.c ~ netflow.1 * strategy + rank.1 + logfund_size.1 + logfund_age.1, data.quant, model = "within", effect = "twoways", index = c("id", "DateQ"))


# 分两组的
load("fund9.RData")
data <- fund.9
data <- data[, yesno := duplicated(alpha_3), keyby = .(id, year, sem)
	][yesno == FALSE, .SD]
# rank.1
data <- data[order(year, sem, category2, sem_return.1)
	][, rank.1 := sequence(.N), keyby = .(year, sem, category2)
	][, returnna := ifelse(is.na(sem_return.1), 1, 0)
	][returnna == 1, rank.1 := sem_return.1]

data <- data[, fund.risk.c := fund.risk.c * 100
	][, fund.sk.c := fund.sk.c * 100]
#对logfund_age分2组
data.age <- data[order(DateQ, logfund_age.1)]

data.age <- data.age[, group := ntile(logfund_age.1, 2), keyby = DateQ
	][, old := ifelse(group == 2, 1, 0)]

liner1 <- plm(fund.risk.c ~ netflow.1 * old + logfund_size.1 + rank.1, data.age, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner2 <- plm(fund.sk.c ~ netflow.1 * old + logfund_size.1 + rank.1, data.age, model = "within", effect = "twoways", index = c("id", "DateQ"))

#对logfund_size
# 分两组的
#对logfund_size分2组
data.size <- data[order(DateQ, logfund_size.1)]

data.size <- data.size[, group := ntile(logfund_size.1, 2), keyby = DateQ
	][, big := ifelse(group == 2, 1, 0)]

liner3 <- plm(fund.risk.c ~ netflow.1 * big + logfund_age.1 + rank.1, data.size, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner4 <- plm(fund.sk.c ~ netflow.1 * big + logfund_age.1 + rank.1, data.size, model = "within", effect = "twoways", index = c("id", "DateQ"))


stargazer(liner1, liner2, liner3, liner4, liner5, liner6, type = "html", out = "C://Users//shenfan//Desktop//data//2//group.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes")))
