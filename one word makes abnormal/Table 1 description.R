# annaul variable
sum <- fread('C:/Users/shenfan/Desktop/one word makes abnormal profit/yubin/with_liq_betas.csv')
sum <- sum[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year := year - 1
	][, .(stock.id, year, illiq, port_beta1, port_beta2, port_beta3, port_beta4, port_beta5, size, bm, mom, INV, GP, analyst_coverage)]

# social average
# add social
IOS <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/社保基金持股比例.xlsx')
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IO.social", "date"))
IOS <- IOS[!is.na(IO.social)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, year := year(date)]
# avg
IOS.avg <- IOS[, .(social.avg = mean(IO.social)), keyby = .(stock.id, year)]

sum <- IOS.avg[sum, on = .(stock.id, year)]

# description
a <- sum[, lapply(colnames(sum[, 3:15]), str_c, ".mean") %>% unlist() := lapply(.SD[, 3:15], mean, na.rm = TRUE)
	][, lapply(colnames(sum[, 3:15]), str_c, ".median") %>% unlist() := lapply(.SD[, 3:15], median, na.rm = TRUE)
	][, lapply(colnames(sum[, 3:15]), str_c, ".std") %>% unlist() := lapply(.SD[, 3:15], sd, na.rm = TRUE)
	][, lapply(colnames(sum[, 3:15]), str_c, ".Q1") %>% unlist() := lapply(.SD[, 3:15], quantile, 0.25, na.rm = TRUE)
	][, lapply(colnames(sum[, 3:15]), str_c, ".Q3") %>% unlist() := lapply(.SD[, 3:15], quantile, 0.75, na.rm = TRUE)
	][, .SD[1]]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")

# return-rf
load('sample.RData')
data <- stock.p[, .(stock.id, hy, num.name, year, month, ret)]
# 各种因子以及alpha 
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
four <- four[, .(year, month, rf)]
# 
data <- four[data, on = .(year, month)]
data <- data[, ret_rf := ret - rf]

a <- data[, lapply(colnames(data[, 8]), str_c, ".mean") %>% unlist() := lapply(.SD[, 8], mean, na.rm = TRUE)
	][, lapply(colnames(data[, 8]), str_c, ".median") %>% unlist() := lapply(.SD[, 8], median, na.rm = TRUE)
	][, lapply(colnames(data[, 8]), str_c, ".std") %>% unlist() := lapply(.SD[, 8], sd, na.rm = TRUE)
	][, lapply(colnames(data[, 8]), str_c, ".Q1") %>% unlist() := lapply(.SD[, 8], quantile, 0.25, na.rm = TRUE)
	][, lapply(colnames(data[, 8]), str_c, ".Q3") %>% unlist() := lapply(.SD[, 8], quantile, 0.75, na.rm = TRUE)
	][, .SD[1]]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")


