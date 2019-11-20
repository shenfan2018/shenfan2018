# clean °üÀ¨logÒÔ¼°Winsorize
# fund
load("variables.RData")

data <- variables[order(id, date)
	][, .(id, date, category1, category2, CSR1, CSR2, raw_return, alpha_capm, alpha_3, alpha_4, alpha_5, alpha_6, Rsquare, vol, numstock, age, size, flow, fee, turnover)]

# Winsorize
data <- data[, colnames(data[, 17:20]) := lapply(.SD[, 17:20], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# log
data <- data[, size := log(size)
	][, year := year(date)
	][, numstock := log(numstock)]

a <- data[, lapply(colnames(data[, 5:20]), str_c, ".mean") %>% unlist() := lapply(.SD[, 5:20], mean, na.rm = TRUE)
	][, lapply(colnames(data[, 5:20]), str_c, ".median") %>% unlist() := lapply(.SD[, 5:20], median, na.rm = TRUE)
	][, lapply(colnames(data[, 5:20]), str_c, ".std") %>% unlist() := lapply(.SD[, 5:20], sd, na.rm = TRUE)
	][, lapply(colnames(data[, 5:20]), str_c, ".Q1") %>% unlist() := lapply(.SD[, 5:20], quantile, 0.25, na.rm = TRUE)
	][, lapply(colnames(data[, 5:20]), str_c, ".Q3") %>% unlist() := lapply(.SD[, 5:20], quantile, 0.75, na.rm = TRUE)
	][, .SD[1]]

write.csv(a, "C:/Users/shenfan/Desktop/csr/table/table1.csv")








