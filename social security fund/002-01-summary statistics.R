# summary statistics
# mutual fund char
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/fundchar.RData")
a <- fund.char[, c("fund_flow", "logfund_size", "logfund_age")]

# social char
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/socialchar.RData")
a <- social.char[, c("social_flow", "logsocial_size", "logsocial_age")]

# company char
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/companychar.RData")
a <- company.char[, c("company_flow", "logcompany_size", "logcompany_size_all", "logcompany_age")]

# fund return
load("fund_ret.RData")
a <- fund.ret[, .(ret.f, ret.f.raw)]

# social return
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/social_ret.RData")
a <- social.ret[, .(ret.s)]

# company return
load("company_ret.RData")
a <- company.ret[, .(ret.c.raw.sub, ret.c.raw.all)]

a <- a[, lapply(colnames(a[, 1:2]), str_c, ".mean") %>% unlist() := lapply(.SD[, 1:2], mean, na.rm = TRUE)
	][, lapply(colnames(a[, 1:2]), str_c, ".median") %>% unlist() := lapply(.SD[, 1:2], median, na.rm = TRUE)
	][, lapply(colnames(a[, 1:2]), str_c, ".std") %>% unlist() := lapply(.SD[, 1:2], sd, na.rm = TRUE)
	][, lapply(colnames(a[, 1:2]), str_c, ".Q1") %>% unlist() := lapply(.SD[, 1:2], quantile, 0.25, na.rm = TRUE)
	][, lapply(colnames(a[, 1:2]), str_c, ".Q3") %>% unlist() := lapply(.SD[, 1:2], quantile, 0.75, na.rm = TRUE)
	][, .SD[1]]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")
