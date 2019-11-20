load("sample.RData")
# year.match 2017匹配2018.7至2019.6
data <- stock.p[, .(stock.id, num.name, year, month, ret)
	][, year.match := ifelse(month > 6, year - 1, year - 2)]

# 加入牛熊
# 1 各种因子以及alpha 
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
four <- four[, .(year, month, mkt_rf)]
# match 牛为1，熊为0
data <- four[data, on = .(year, month)]
data <- data[, mktrf := ifelse(mkt_rf > 0, 1, 0)]

# 2 bull or bear 
#######################
## bear/bull market (monthly)
# bull market when the month's past 12 month cumulative market risk premium is positive
bb <- four[, .(year, month, mkt_rf, rf)]

#先变月度
bb <- bb[, tag := seq(1:(.N))
	][, mkt_rf2 := mkt_rf + rf + 1
	][, rf2 := rf + 1]

reg.roll <- list()
for (i in 13:299) {
	reg.roll[[i]] <- bb[tag >= i - 12 & tag <= i - 1
	][, .(mkt = prod(mkt_rf2) - 1, rf = prod(rf2) - 1)]

	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:299, reg.roll)
roll <- roll[13:299]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
bb <- reg.cof[bb, on = .(tag)
	][, bull := ifelse(mkt > rf, 1, 0)
	][year > 2003
	][, .(year, month, bull)]

########################################
data <- bb[data, on = .(year, month)]


## 加入 liquid
liq <- fread('C:/Users/shenfan/Desktop/one word makes abnormal profit/yubin/with_liq_betas.csv')
liq <- liq[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year.match := year - 1
	][, .(stock.id, year.match, illiq, port_beta2, port_beta3, port_beta4, port_beta5, size, bm)]
# match
data <- liq[data, on = .(stock.id, year.match)]

data <- data[, logsize := log(size)
	][, logbm := log(bm)
	][, namedummy := ifelse(num.name == 3, 1, 0)]

write.csv(data, "C://Users//shenfan//Desktop//onewordliq.csv")

#lm(ret ~ illiq + port_beta2 + port_beta3 + port_beta4 + port_beta5, data[num.name == 3]) %>% summary()
#lm(ret ~ illiq + port_beta2 + port_beta3 + port_beta4 + port_beta5, data[num.name == 4]) %>% summary()


# sort
# beta2-4, beta5, illiq
# fama macbeth 的数据结构
table <- data
table <- table[!is.na(illiq)
	][, lapply(colnames(table[, 3:7]), str_c, ".g") %>% unlist() := lapply(.SD[, 3:7], ntile, 5), keyby = .(year, month, num.name)
	][, .(stock.id, num.name, year, month, bull, mktrf, ret, illiq.g, port_beta2.g, port_beta3.g, port_beta4.g, port_beta5.g)]

table = melt(table, id.vars = 1:7, measure.vars = 8:12)

# 这里改bull or mktrf
table <- table[!is.na(value)
	][, .(ret = mean(ret)), keyby = .(year, month, bull, num.name, variable, value)]

# mean
a <- table[, .(ret = mean(ret)), keyby = .(bull, num.name, variable, value)]
write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# difference
a <- table[order(variable, year, month, num.name, value)
	][, ret2 := shift(ret, n = 5, fill = NA, type = "lead"), keyby = .(variable)
	][num.name == 3
	][, dif := ret - ret2
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic), keyby = .(bull, variable, value)]
write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")


# illiquid analysis regression
# 牛市
lm(ret ~ illiq + port_beta5, data[num.name == 3 & mktrf == 1]) %>% summary()
lm(ret ~ illiq + port_beta5, data[num.name == 4 & mktrf == 1]) %>% summary()

lm(ret ~ illiq + port_beta5, data[num.name == 3 & mktrf == 0]) %>% summary()
lm(ret ~ illiq + port_beta5, data[num.name == 4 & mktrf == 0]) %>% summary()