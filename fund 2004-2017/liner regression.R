# performance and risk taking change
load("fund9.RData")
data <- fund.9
# 这里再加一个expense ratio
expense <- read_excel("C://Users//shenfan//Desktop//基金经理//data//2//管理费率.xlsx")
expense <- as.data.table(expense)
setnames(expense, 1:3, c("code", "name", "expense_ratio"))
expense <- expense[, c("code", "expense_ratio")]
# 加入
data <- expense[data, on = .(code), nomatch = 0]


data <- data[, yesno := duplicated(alpha_3), keyby = .(id, year, sem)
	][yesno == FALSE, .SD]

data <- data[order(id, year, sem)
	][, sem_return.d1 := shift(sem_return, n = 1, fill = NA, type = "lead"), keyby = id
	][order(year, sem, category2, - sem_return.d1)
	][, rank.d1 := sequence(.N), keyby = .(year, sem, category2)
	][, returnna := ifelse(is.na(sem_return.d1), 1, 0)
	][returnna == 1, rank.d1 := sem_return.d1
	][order(year, sem, category2, - sem_return.1)
	][, rank.1 := sequence(.N), keyby = .(year, sem, category2)
	][, returnna := ifelse(is.na(sem_return.1), 1, 0)
	][returnna == 1, rank.1 := sem_return.1]


# risk taking change driven by net flow，数值太小了
data <- data[, fund.risk.c := fund.risk.c * 100
	][, fund.sk.c := fund.sk.c * 100]

write.csv(data, "C://Users//shenfan//Desktop//riskshifting.csv")

liner1 <- plm(fund.risk.c ~ netflow.1 + rank.1 + logfund_size.1 + logfund_age.1 + expense_ratio, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner2 <- plm(fund.sk.c ~ netflow.1 + rank.1 + logfund_size.1 + logfund_age.1 + expense_ratio, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner3 <- plm(fund.risk.c ~ inflow.1 + rank.1 + logfund_size.1 + logfund_age.1 + expense_ratio, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner4 <- plm(fund.sk.c ~ inflow.1 + rank.1 + logfund_size.1 + logfund_age.1 + expense_ratio, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner5 <- plm(fund.risk.c ~ outflow.1 + rank.1 + logfund_size.1 + logfund_age.1 + expense_ratio, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner6 <- plm(fund.sk.c ~ outflow.1 + rank.1 + logfund_size.1 + logfund_age.1 + expense_ratio, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, liner4, liner5, liner6, type = "html", out = "C://Users//shenfan//Desktop//基金经理//data//2//net flow2.doc", report = ('vc*t'), add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes")))



## performance的

liner1 <- plm(rank.d1 ~ fund.risk.c + rank + logfund_size + logfund_age + expense_ratio, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner2 <- plm(rank.d1 ~ fund.sk.c + rank + logfund_size + logfund_age + expense_ratio, data, model = "within", effect = "twoways", index = c("id", "DateQ"))


stargazer(liner1, liner2, type = "html", out = "C://Users//shenfan//Desktop//基金经理//data//2//performance2.doc", report = ('vc*t'), add.lines = list(c("fund", "yes", "yes"), c("time", "yes", "yes")))

