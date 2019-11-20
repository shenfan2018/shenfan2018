# 这里想做那个figure
load("fund9.RData")

# 选出low low的
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

# 这是一期
# data <- data[order(id, year, sem)
#	][, str_c(colnames(data)[4:7], str_c(".d", 1)) := lapply(.SD[, 3:6], shift, n = 1, fill = NA, type = "lead"), keyby = id]

# 这是循环四期
for (i in 1:4) {
	data <- data[, str_c(colnames(data)[4:7], str_c(".d", i)) := lapply(.SD[, 3:6], shift, n = i, fill = NA, type = "lead"), keyby = id]
}

low <- data[per.tb == 5 & flow.tb == 1, .SD]

# 区别risk change大的和小的
low <- low[, risk.tb := ntile(fund.risk.c, 2)
	][, str_c(colnames(data)[58:73], str_c(".mean")) := lapply(.SD[, 58:73], mean, na.rm = TRUE), keyby = risk.tb]

figure <- low

figure <- figure[, yesno := duplicated(alpha_5.d4.mean), keyby = .(risk.tb)
	][yesno == FALSE, .SD
	][, 75:90]

write.csv(figure, "C://Users//shenfan//Desktop//mydatam.csv")

a <- low[, .(alpha_3.mean = mean(alpha_3), alpha_capm.mean=mean(alpha_capm)), keyby = risk.tb]

figure2 <- read_excel("C://Users//shenfan//Desktop//222.xlsx")
figure2 <- as.data.table(figure2)
setnames(figure2, 1, "time")
low <- figure2[group == "low", .SD]
high <- figure2[group == "high",.SD]


ggplot(low, aes(x = time)) + geom_line(aes(y = alpha_capm, colour = "var0")) + geom_line(aes(y = alpha_3, colour = "var1")) + geom_line(aes(y = alpha_4, colour = "var2")) + geom_line(aes(y = alpha_5, colour = "var3"))

ggplot(high, aes(x = time)) + geom_line(aes(y = alpha_capm, colour = "var0")) + geom_line(aes(y = alpha_3, colour = "var1")) + geom_line(aes(y = alpha_4, colour = "var2")) + geom_line(aes(y = alpha_5, colour = "var3"))