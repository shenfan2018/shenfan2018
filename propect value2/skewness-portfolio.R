# 
load("skewness.RData")
skewness <- skewness[, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)]

# 加入时间
date <- read_excel("C:/Users/shenfan/Desktop/prospect value/date.xlsx")
date <- as.data.table(date)
date <- date[, year := year(date)
	][, month := month(date)]
skewness <- date[skewness, on = .(year, month)]

################# month
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
pf <- portfolio
pf <- pf[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(id, name, year, sem, stock.id, MarketValue)]

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.NAV <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, sem, month)]

mutual <- portfolio[order(id)
	][, .(id)
	][, .(id = unique(id))]
mutual <- mutual[["id"]]

# 加入时间
date <- read_excel("C:/Users/shenfan/Desktop/prospect value/date.xlsx")
date <- as.data.table(date)
date <- date[, year := year(date)
	][, month := month(date)
	][, date := as.Date(date)
	][date > as.Date("2003-12-31")]
date <- date[["date"]]

stock <- portfolio[order(stock.id)
	][, .(stock.id)
	][, .(stock.id = unique(stock.id))]
stock <- stock[["stock.id"]]

s.d <- CJ(stock.id = stock, date = date)
s.d <- s.d[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)]

# 这个版本的stock proportion向后一期 一期为半年
data <- list()
for (i in 1:693) {
	fund1 <- mutual[i]
	data[[i]] <- s.d[, id := fund1]
	# 这里滞后
	data[[i]] <- pf[s.d, on = .(stock.id, id, year, sem), nomatch = 0
	][, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)
	][, .(id, year, sem, stock.id, MarketValue)]
	data[[i]] <- skewness[data[[i]], on = .(stock.id, year, sem), nomatch = 0
	][!is.nan(skewness)
	][, stock.p := MarketValue / sum(MarketValue, na.rm = TRUE), keyby = .(date)
	][, .(SKEWNESS = sum(stock.p * skewness)), keyby = .(date)
	][, id := fund1]

	rbindlist(data)
}

roll <- data.table(i = 1:693, data)
roll <- roll[1:693]
syf <- roll[, rbindlist(.SD[['data']]), by = i]

syf <- syf[, year := year(date)
	][, month := month(date)
	][, .(id, year, month, date, SKEWNESS)]


save(syf, file = "fund-SKEWNESS.RData")

#################################################### single sorting
load("fund-SKEWNESS.RData")

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.NAV <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)]

data <- data.NAV[syf, on = .(id, year, month)]

data <- data[order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, skewness.g := ntile(SKEWNESS, 5), keyby = .(date)]

TK.ret <- data[!is.na(month_return.1)
	][, .(ret = mean(month_return.1)), keyby = .(date, skewness.g)
	][!is.na(ret)]

low <- TK.ret[skewness.g == 1]
high <- TK.ret[skewness.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.ret[skewness.g == 5, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)


data[, felm(month_return.1 ~ SKEWNESS + month_return | id + year)] %>% summary()


######################################### double sorting
load("fund-SKEWNESS.RData")

load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.NAV <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)]

data <- data.NAV[syf, on = .(id, year, month)]

data <- data[, date := as.Date(date)
	][date > as.Date("2005-01-01")
	][order(id, date)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, month_return.g := ntile(month_return, 5), keyby = .(date)
	][, skewness.g := ntile(SKEWNESS, 5), keyby = .(date, month_return.g)]

TK.g <- data[, .(ret = mean(month_return.1)), keyby = .(date, skewness.g, month_return.g)
	][!is.na(ret)]

low <- TK.g[skewness.g == 1 & month_return.g == 1]
high <- TK.g[skewness.g == 5 & month_return.g == 1]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[skewness.g == 5 & month_return.g == 1, ret])
t.test(dif[, dif])

fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)

## reg
reg <- data[, felm(month_return.1 ~ SKEWNESS + month_return | id + year)]

stargazer(reg, type = "html", out = "C:/Users/shenfan/Desktop/prospect value/tables/skewness and performance.doc", add.lines = list(c("Fund", "yes"), c("year", "yes")))