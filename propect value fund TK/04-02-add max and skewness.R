# add skewness and max

load("monthlydata.RData")
data <- fund.TK

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
library(moments)
#ХыАн
skewness <- data.NAV[, year := year(date)
	][, month := month(date)
	][, .(skewness = skewness(AdjustedNAVGrowth, na.rm = TRUE)), keyby = .(id, year, month)]

data <- skewness[data, on = .(id, year, month)]

max <- data.NAV[order(id, year, month, - AdjustedNAVGrowth)
	][, .SD[1], keyby = .(id, year, month)
	][, .(id, year, month, AdjustedNAVGrowth)]
setnames(max, "AdjustedNAVGrowth", "max")

data <- max[data, on = .(id, year, month)]

# four_alpha
four <- fread("C:/Users/shenfan/Desktop/Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, .(year, month, rf)]

data <- mkt[data, on = .(year, month)
	][, ret_rf := month_return - rf]

load("fundfactor.RData")
data <- alpha[data, on = .(id, year, month)]

data <- data[, ret_rf.1 := shift(ret_rf, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, alpha_four.1 := shift(alpha_four, n = 1, fill = NA, type = "lead"), keyby = .(id)]

data <- data[!is.nan(TK)
	][, netflow := netflow / 100
	][, churn.rate := churn.rate / 100]

write.csv(data, "C://Users//shenfan//Desktop//maxskewness.csv")

reg1 <- pmg(month_return.1 ~ TK, data, index = c("date", "id"))

reg2 <- pmg(month_return.1 ~ TK + logfund_size + logfund_age, data, index = c("date", "id"))

reg3 <- pmg(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate, data, index = c("date", "id"))

reg4 <- pmg(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate + month_return, data, index = c("date", "id"))

reg5 <- pmg(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate + month_return + netflow, data, index = c("date", "id"))

reg6 <- pmg(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate + month_return + netflow + skewness, data, index = c("date", "id"))

reg7 <- pmg(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate + month_return + netflow + max, data, index = c("date", "id"))

reg8 <- pmg(month_return.1 ~ TK + logfund_size + logfund_age + churn.rate + month_return + netflow + skewness + max, data, index = c("date", "id"))

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, type = "html", out = "C:/Users/shenfan/Desktop/prospect value/new/skewness.doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")))
