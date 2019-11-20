load ("cleanmonthly.RData")

a <- data[order(year, id)
	][, sample := .N, keyby = year
	][, mean.p.TK := mean(p.TK, na.rm = TRUE), keyby = year
	][, mean.TK := mean(TK, nan.rm = TRUE), keyby = year
	][, mean.logfund.size := mean(logfund_size, na.rm = TRUE), keyby = year
	][, mean.logfund.age := mean(logfund_age), keyby = year
	][, mean.netflow := mean(netflow, na.rm = TRUE), keyby = year
	][, mean.max := mean(max, na.rm = TRUE), keyby = year
	][, mean.skewness := mean(skewness, na.rm = TRUE), keyby = year
	][, mean.return := mean(month_return), keyby = year
	][, mean.churn.rate := mean(churn.rate, na.rm = TRUE), keyby = year
	][, yesno := duplicated(year)
	][yesno == FALSE, .SD]

b <- data[, .(id = unique(id)), keyby = .(year)
	][, .(sample = .N), keyby = year]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

##
a <- data[, c("netflow", "month_return", "logfund_size", "logfund_age", "TK", "p.TK", "max", "skewness", "churn.rate")]
a <- a[, lapply(colnames(a[, 1:9]), str_c, ".mean") %>% unlist() := lapply(.SD[, 1:9], mean, na.rm = TRUE)
	][, lapply(colnames(a[, 1:9]), str_c, ".median") %>% unlist() := lapply(.SD[, 1:9], median, na.rm = TRUE)
	][, lapply(colnames(a[, 1:9]), str_c, ".std") %>% unlist() := lapply(.SD[, 1:9], sd, na.rm = TRUE)
	][, lapply(colnames(a[, 1:9]), str_c, ".Q1") %>% unlist() := lapply(.SD[, 1:9], quantile, 0.25, na.rm = TRUE)
	][, lapply(colnames(a[, 1:9]), str_c, ".Q3") %>% unlist() := lapply(.SD[, 1:9], quantile, 0.75, na.rm = TRUE)
	][, .SD[1]]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# ±Ì2
# pearsonºÏ—È
a <- data[, c("netflow", "month_return", "logfund_size", "logfund_age", "TK", "p.TK", "max", "skewness")]

b <- cor(a, use = "complete.obs", method = "pearson")
write.csv(b, "C://Users//shenfan//Desktop//mydatam.csv")
