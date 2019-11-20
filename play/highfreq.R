library(data.table)

a <- fread("C:/Users/shenfan/source/repos/Python-play/crawlerhighfrequence/ynhg.csv", encoding = "UTF-8")

a <- a[, .(time, price)
	][order(time)]

syf <- list()
for (i in 10) {
	syf[[i]] <- a[, {
		j <- shift(price, n = i, fill = NA, type = "lead")
	},
	]
}