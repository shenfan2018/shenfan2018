#描述性统计
# 表1
load("monthlydata.RData")
data <- fund.TK

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
library(moments)
#整理
skewness <- data.NAV[, year := year(date)
	][, month := month(date)
	][, .(skewness = skewness(AdjustedNAVGrowth, na.rm = TRUE)), keyby = .(id, year, month)]

data <- skewness[data, on = .(id, year, month)]

max <- data.NAV[order(id, year, month, - AdjustedNAVGrowth)
	][, .SD[1], keyby = .(id, year, month)
	][, .(id, year, month, AdjustedNAVGrowth)]
setnames(max, "AdjustedNAVGrowth", "max")

data <- max[data, on = .(id, year, month)]

data <- data[!is.nan(TK)
	][, netflow := netflow / 100
	][, churn.rate := churn.rate / 100]

# 极端值处理
# netflow
a1 <- quantile(data[, netflow], 0.99, na.rm = TRUE)
a2 <- quantile(data[, netflow], 0.01, na.rm = TRUE)
data <- data[order(netflow)
	][, extremum := ifelse(netflow > a1, 2, ifelse(netflow < a2, 1, 0))
	][extremum == 1, netflow := netflow[.N]
	][extremum == 2, netflow := netflow[1]]

# max
a1 <- quantile(data[, max], 0.99, na.rm = TRUE)
a2 <- quantile(data[, max], 0.01, na.rm = TRUE)
data <- data[order(max)
	][, extremum := ifelse(max > a1, 2, ifelse(max < a2, 1, 0))
	][extremum == 1, max := max[.N]
	][extremum == 2, max := max[1]]

# churn.rate
a1 <- quantile(data[, churn.rate], 0.99, na.rm = TRUE)
a2 <- quantile(data[, churn.rate], 0.01, na.rm = TRUE)
data <- data[order(churn.rate)
	][, extremum := ifelse(churn.rate > a1, 2, ifelse(churn.rate < a2, 1, 0))
	][extremum == 1, churn.rate := churn.rate[.N]
	][extremum == 2, churn.rate := churn.rate[1]]

data <- data[order(id, year, month)]
save(data, file = "cleanmonthly.RData")

