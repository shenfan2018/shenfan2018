# try
# raw return
load('fund-NAV.RData')

# alpha
factor <- fread("C:/Users/shenfan/Desktop/The payoff of investing in mutual funds with a high CSR score portfolio Evidence from China/基金data/更新至2019-07-05-three_four_five_factor_daily/three_four_five_factor_daily/fivefactor_daily.csv")
setnames(factor, "trddy", "date")
factor <- factor[, date := as.Date(date)]

NAV <- factor[data.NAV, on = .(date), nomatch = 0
	][!is.na(AdjustedNAVGrowth)
	][, ret_rf := AdjustedNAVGrowth - rf
	][, year := year(date)
	][, month := month(date)
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][, .(raw_return = prod(AdjustedNAVGrowth + 1) - 1, alpha_capm = coef(lm(ret_rf ~ mkt_rf))[1], alpha_3 = coef(lm(ret_rf ~ mkt_rf + smb + hml))[1], alpha_4 = coef(lm(ret_rf ~ mkt_rf + smb + hml + umd))[1], alpha_5 = coef(lm(ret_rf ~ mkt_rf + smb + hml + rmw + cma))[1]), keyby = .(id, year, sem)
	][year > 2008]


# category 01-update data to 2018 category
data <- category[NAV, on = .(id), nomatch = 0]

# csr 加 portfolio

# csr
csr <- fread("C:/Users/shenfan/Desktop/The payoff of investing in mutual funds with a high CSR score portfolio Evidence from China/CSR_09to18.csv")
csr <- csr[, stkcd := as.character(stkcd)
	][, stkcd := str_pad(stkcd, 6, side = "left", pad = "0")
	][, .(stkcd, year, CSR)]

# portfolio
load("portfolio.RData")
setnames(portfolio, "stock.id", "stkcd")
portfolio <- portfolio[, year := year(date)]

# match
csr <- csr[portfolio, on = .(stkcd, year)]

# value weighted
csr <- csr[, sum := sum(MarketValue), keyby = .(id, date)
	][, vw := MarketValue / sum
	][, sum := NULL
	][, .(CSR1 = sum(CSR * vw, na.rm = TRUE), CSR2 = sum(CSR * Proportion / 100, na.rm = TRUE)), keyby = .(id, date, year)
	][year > 2008]

# csr滞后？
csr <- csr[, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)
	][, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]


# match
data <- csr[data, on = .(id, year, sem), nomatch = 0]


# 分组
a <- data[, CSR1g := ntile(CSR1, 5), keyby = .(year, sem)
	][, CSR2g := ntile(CSR2, 5), keyby = .(year, sem)
	][, lapply(.SD[, 8:12], mean, na.rm = TRUE), keyby = .(year, sem, CSR1g)]

# mean
b <- a[, lapply(.SD[, 3:7], mean, na.rm = TRUE), keyby = .(CSR1g)]

# difference
table = melt(a, id.vars = 1:3, measure.vars = 4:8)

table <- table[, value.2 := shift(value, n = 4, fill = NA, type = "lead")
	][CSR1g == 1
	][, dif := value.2 - value
	][, .(ret = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3]), keyby = .(variable)]

write.csv(table, "C://Users//shenfan//Desktop//aaa.csv")


# plm
plm(CSR1 ~ alpha_3, data, model = "within", effect = "twoways", index = c("id", "date")) %>% summary()

lm(CSR1 ~ alpha_3, data) %>% summary()
