load("monthlydata.RData")
data <- fund.TK

# 变成季度
data <- data[, .(TK = mean(TK)), keyby = .(id, year, quarter)]

# ret
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.q <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(quarter_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter)]

data <- data.q[data, on = .(id, year, quarter)]

# T期的TK对T期的netflow
syf <- data
syf <- syf[!is.nan(TK)
	][!is.na(netflow)]

#clean
#对netflow
a1 <- quantile(syf[, netflow], 0.99, na.rm = TRUE)
a2 <- quantile(syf[, netflow], 0.01, na.rm = TRUE)
syf <- syf[order(netflow)
	][, extremum := ifelse(netflow > a1, 2, ifelse(netflow < a2, 1, 0))
	][extremum == 1, netflow := netflow[.N]
	][extremum == 2, netflow := netflow[1]]

#对inflow
a1 <- quantile(syf[, inflow], 0.99, na.rm = TRUE)
a2 <- quantile(syf[, inflow], 0.01, na.rm = TRUE)
syf <- syf[order(inflow)
	][, extremum := ifelse(inflow > a1, 2, ifelse(inflow < a2, 1, 0))
	][extremum == 1, inflow := inflow[.N]
	][extremum == 2, inflow := inflow[1]]

#对outflow
a1 <- quantile(syf[, outflow], 0.99, na.rm = TRUE)
a2 <- quantile(syf[, outflow], 0.01, na.rm = TRUE)
syf <- syf[order(outflow)
	][, extremum := ifelse(outflow > a1, 2, ifelse(outflow < a2, 1, 0))
	][extremum == 1, outflow := outflow[.N]
	][extremum == 2, outflow := outflow[1]]

###############################################################################sorting
syf <- syf[order(id, year, quarter)
	][, TK.g := ntile(TK, 5), keyby = .(year, quarter)]

TK.g <- syf[, .(inflow = mean(inflow), outflow = mean(outflow), netflow = mean(netflow)), keyby = .(year, quarter, TK.g)]


low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(year, quarter)
	][, dif := i.netflow - netflow]

t.test(TK.g[TK.g == 5, netflow])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


# T期的TK对T+1的netflow
syf <- data
syf <- syf[, TK.1 := shift(TK, n = 1, fill = NA, type = "lag"), keyby = .(id)
	][!is.nan(TK.1)
	][!is.na(netflow)]

#clean
#对netflow
a1 <- quantile(syf[, netflow], 0.99, na.rm = TRUE)
a2 <- quantile(syf[, netflow], 0.01, na.rm = TRUE)
syf <- syf[order(netflow)
	][, extremum := ifelse(netflow > a1, 2, ifelse(netflow < a2, 1, 0))
	][extremum == 1, netflow := netflow[.N]
	][extremum == 2, netflow := netflow[1]]

#对inflow
a1 <- quantile(syf[, inflow], 0.99, na.rm = TRUE)
a2 <- quantile(syf[, inflow], 0.01, na.rm = TRUE)
syf <- syf[order(inflow)
	][, extremum := ifelse(inflow > a1, 2, ifelse(inflow < a2, 1, 0))
	][extremum == 1, inflow := inflow[.N]
	][extremum == 2, inflow := inflow[1]]

#对outflow
a1 <- quantile(syf[, outflow], 0.99, na.rm = TRUE)
a2 <- quantile(syf[, outflow], 0.01, na.rm = TRUE)
syf <- syf[order(outflow)
	][, extremum := ifelse(outflow > a1, 2, ifelse(outflow < a2, 1, 0))
	][extremum == 1, outflow := outflow[.N]
	][extremum == 2, outflow := outflow[1]]

syf <- syf[order(id, year, quarter)
	][, TK.g := ntile(TK.1, 5), keyby = .(year, quarter)]

TK.g <- syf[, .(inflow = mean(inflow), outflow = mean(outflow), netflow = mean(netflow)), keyby = .(year, quarter, TK.g)]


low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(year, quarter)
	][, dif := i.outflow - outflow]

t.test(TK.g[TK.g == 5, outflow])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)



