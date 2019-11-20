load("data2.RData")
data <- fund.3

# 表4 T-1期rank和T-1期的flow
data <- data[order(year, sem, category2, - sem_return.1)
	][, rank.1 := sequence(.N), by = .(year, sem, category2)]

# 这里啊 要把NA的去掉哦(这里还是考虑到了的）
data <- data[!is.na(sem_return.1)
	][, per.tb := ntile(rank.1, 5), keyby = .(year, sem)]

data <- data[per.tb == 1, flow.tb := ntile(netflow.1, 5)
	][per.tb == 2, flow.tb := ntile(netflow.1, 5)
	][per.tb == 3, flow.tb := ntile(netflow.1, 5)
	][per.tb == 4, flow.tb := ntile(netflow.1, 5)
	][per.tb == 5, flow.tb := ntile(netflow.1, 5)]

data[, .N, keyby = .(per.tb, flow.tb)]

#1-5从小到大, 所以对于rank的话，即per.tb=1,rank最高
t.test(data[per.tb == 5, "zpf"])

t.test(data[flow.tb == 5, "zpf"])

t.test(data[per.tb == 5 & flow.tb == 5, "zpf"])

t.test(data[per.tb == 1 & flow.tb == 5, "zpf"], data[per.tb == 5 & flow.tb == 5, "zpf"], paired = FALSE)

t.test(data[, "zpf"])

t.test(data[per.tb == 1, "zpf"], data[per.tb == 5, "zpf"], paired = FALSE)

t.test(data[flow.tb == 5, "zpf"], data[flow.tb == 1, "zpf"], paired = FALSE)

