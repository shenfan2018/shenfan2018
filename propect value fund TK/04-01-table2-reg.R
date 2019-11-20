load("monthlydata.RData")
data <- fund.TK

data <- data[, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]

data <- data[!is.nan(TK)
	][, netflow := netflow / 100
	][, churn.rate := churn.rate / 100]

# mkt
# four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
# four <- as.data.table(four)
# mkt <- four[, mkt := mkt_rf + rf
#	][, .(year, month, mkt, rf)]

# data <- mkt[data, on = .(year, month)]

# write.csv(data, "C://Users//shenfan//Desktop//data.csv")

reg1 <- plm(month_return.1 ~ TK, data, model = "within", effect = "twoways", index = c("date", "id"))

reg2 <- plm(month_return.1 ~ TK + logfund_size + logfund_age, data, model = "within", effect = "twoways", index = c("date", "id"))

reg3 <- plm(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate, data, model = "within", effect = "twoways", index = c("date", "id")) 

reg4 <- plm(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate + month_return , data, model = "within", effect = "twoways", index = c("date", "id"))

reg5 <- plm(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate + month_return + netflow, data, model = "within", effect = "twoways", index = c("date", "id"))

stargazer(reg1, reg2, reg3, reg4, reg5, type = "html", out = "C:/Users/shenfan/Desktop/prospect value/new/reg1.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes")))



# fama macbeth
pmg(month_return.1 ~ TK, data, index = c("date", "id")) %>% summary()

pmg(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate + month_return, data, index = c("date", "id")) %>% summary()

