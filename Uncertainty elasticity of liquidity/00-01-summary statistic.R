# add mutual social 
load('uel2.RData')

load('IOquarterdeal.RData')

a <- uel[, .(stock.id, year, IOS, IOF, IOI, IOQ, prc)]


a <- a[, lapply(colnames(a[, 3:7]), str_c, ".mean") %>% unlist() := lapply(.SD[, 3:7], mean, na.rm = TRUE)
	][, lapply(colnames(a[, 3:7]), str_c, ".median") %>% unlist() := lapply(.SD[, 3:7], median, na.rm = TRUE)
	][, lapply(colnames(a[, 3:7]), str_c, ".std") %>% unlist() := lapply(.SD[, 3:7], sd, na.rm = TRUE)
	][, lapply(colnames(a[, 3:7]), str_c, ".Q1") %>% unlist() := lapply(.SD[, 3:7], quantile, 0.25, na.rm = TRUE)
	][, lapply(colnames(a[, 3:7]), str_c, ".Q3") %>% unlist() := lapply(.SD[, 3:7], quantile, 0.75, na.rm = TRUE)
	][, .SD[1]]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")
