load('USfundfactor.RData')
# 要运行14-02
alpha <- alpha[, year := as.numeric(substring(Date, 1, 4))
	][, month := as.numeric(substring(Date, 5, 6))
	][, fund.id := as.character(id)
	][, id := NULL]

fund.TK <- fund.TK[, year := as.numeric(year)
	][, month := as.numeric(month)]

# match alpha include raw return,
data <- fund.TK[alpha, on = .(fund.id, year, month)]

# year > 1991, 1991-2012
data <- data[year > 1991
	][, Date := NULL]

######################################################################
# 参照11-02，还是用这个保持一致
# 所有都往上移动
data <- data[, lapply(colnames(data[, 5:10]), str_c, ".1") %>% unlist() := lapply(.SD[, 4:9], shift, n = 1L, type = "lead"), keyby = .(fund.id)
	][!is.na(TK)
	][, TK.g := ntile(TK, 5), keyby = .(year, month)
	][, .(fund.id, year, month, TK, TK.g, month_return.1, alpha_CAPM.1, alpha_three.1, alpha_four.1, alpha_five.1, alpha_six.1)]

data <- melt(data, id.vars = colnames(data[, 1:5]), measures.vars = colnames(data[, 6:11]))

group <- data[, .(ret = mean(value, na.rm = TRUE)), keyby = .(year, month, variable, TK.g)
	][, .(ret = mean(ret, na.rm = TRUE)), keyby = .(variable, TK.g)]

write.csv(group, "C://Users//shenfan//Desktop//mydatam.csv")

# dif
group <- data[, .(ret = mean(value, na.rm = TRUE)), keyby = .(year, month, variable, TK.g)]

low <- group[TK.g == 1]
high <- group[TK.g == 5]
dif <- low[high, on = .(year, month, variable)
	][, dif := i.ret - ret
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable)]

write.csv(dif, "C://Users//shenfan//Desktop//mydatam2.csv")


########################################################################### 换一个思路实施
# match alpha include raw return,
data <- fund.TK[alpha, on = .(fund.id, year, month)]

# year > 1991, 1991-2012
data <- data[year > 1991
	][, Date := NULL]

# 把TK往前就可以了，fund Tk group to 1-5
data <- data[, TK.1:= shift(TK, n = 1, fill = NA, type = 'lag'), keyby = .(fund.id)
	][, TK.g := ntile(TK.1, 5), keyby = .(year, month)
	][, .(fund.id, year, month, TK.g, TK.1, month_return, alpha_CAPM, alpha_three, alpha_four, alpha_five, alpha_six)
	][!is.na(TK.g)
	][, lapply(.SD[, 3:8], mean, na.rm = TRUE), keyby = .(year, month, TK.g)]
#	][, lapply(.SD[, 3:8], mean, na.rm = TRUE), keyby = .(TK.g)]

#write.csv(data, "C://Users//shenfan//Desktop//11.csv")

data <- melt(data, id.vars = colnames(data[, 1:3]), measures.vars = colnames(data[, 3:8]))
# difference
low <- data[TK.g == 1]
high <- data[TK.g == 5]
dif <- low[high, on = .(year, month, variable)
	][, dif := i.value - value
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate, t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable)]


write.csv(dif, "C://Users//shenfan//Desktop//11.csv")


########################################################################################################################################### new
# 接上面
data <- data[!is.na(TK)
	][, TK.g := ntile(TK, 5), keyby = .(year, month)
	][, .(fund.id, year, month, month_return, TK.g)
	][order(fund.id, year, month)
	][, TK.g.1 := shift(TK.g, n = 1, fill = NA, type = "lag"), keyby = .(fund.id)
	][!is.na(TK.g.1)
	][, .(ret = mean(month_return, na.rm = TRUE)), keyby = .(year, month, TK.g.1)]

# factor
# factor
factor1 <- fread('C:/Users/shenfan/Desktop/prospect value/data US/factor1.csv')
factor1 <- factor1[, Date := substring(dateff, 1, 6)
	][, .(Date, UMD)]

factor2 <- fread('C:/Users/shenfan/Desktop/prospect value/data US/factor2.csv')
factor2 <- factor2[, Date := as.character(Date)
	][, colnames(factor2[, 2:7]) := .SD[, 2:7] / 100]
# match
factor <- factor1[factor2, on = .(Date), nomatch = 0]
rm(factor1, factor2)

factor <- factor[, year := as.numeric(substring(Date, 1, 4))
	][, month := as.numeric(substring(Date, 5, 6))]

# match factor
data <- factor[data, on = .(year, month)]

data <- data[, rit := ret - RF]

a <- data[, .(raw_return = mean(ret), raw_t = t.test(ret)$statistic, alpha_capm = coef(lm(rit ~ Mkt_RF))[1], t_capm = summary(lm(rit ~ Mkt_RF))$coef[1, 3], alpha_three = coef(lm(rit ~ Mkt_RF + SMB + HML))[1], t_three = summary(lm(rit ~ Mkt_RF + SMB + HML))$coef[1, 3], alpha_four = coef(lm(rit ~ Mkt_RF + SMB + HML + UMD))[1], t_four = summary(lm(rit ~ Mkt_RF + SMB + HML + UMD))$coef[1, 3], alpha_five = coef(lm(rit ~ Mkt_RF + SMB + HML + RMW + CMA))[1], t_five = summary(lm(rit ~ Mkt_RF + SMB + HML + RMW + CMA))$coef[1, 3], alpha_six = coef(lm(rit ~ Mkt_RF + SMB + HML + RMW + CMA + UMD))[1], t_six = summary(lm(rit ~ Mkt_RF + SMB + HML + RMW + CMA + UMD))$coef[1, 3]), keyby = .(TK.g.1)]

write.csv(a, "C://Users//shenfan//Desktop//mydata.csv")


# dif
# dif
a <- data[, rit2 := shift(rit, n = 4, fill = NA, type = "lead")
	][, dif := rit2 - rit
	][TK.g.1 == 1]

a <- a[, .(raw_return = t.test(dif)$estimate, raw_t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], alpha_capm = coef(lm(dif ~ Mkt_RF))[1], t_capm = summary(lm(dif ~ Mkt_RF))$coef[1, 3], alpha_three = coef(lm(dif ~ Mkt_RF + SMB + HML))[1], t_three = summary(lm(dif ~ Mkt_RF + SMB + HML))$coef[1, 3], alpha_four = coef(lm(dif ~ Mkt_RF + SMB + HML + UMD))[1], t_four = summary(lm(dif ~ Mkt_RF + SMB + HML + UMD))$coef[1, 3], alpha_five = coef(lm(dif ~ Mkt_RF + SMB + HML + RMW + CMA))[1], t_five = summary(lm(dif ~ Mkt_RF + SMB + HML + RMW + CMA))$coef[1, 3], alpha_six = coef(lm(dif ~ Mkt_RF + SMB + HML + RMW + CMA + UMD))[1], t_six = summary(lm(dif ~ Mkt_RF + SMB + HML + RMW + CMA + UMD))$coef[1, 3]), keyby = .(TK.g.1)]
