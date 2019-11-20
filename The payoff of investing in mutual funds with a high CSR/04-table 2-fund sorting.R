# table2

load("variables.RData")

# 极端值
a1 <- quantile(variables[, turnover], 0.99, na.rm = TRUE)
a2 <- quantile(variables[, turnover], 0.01, na.rm = TRUE)
variables <- variables[order(turnover)
	][, extremum := ifelse(turnover > a1, 2, ifelse(turnover < a2, 1, 0))
	][extremum == 1, turnover := turnover[.N]
	][extremum == 2, turnover := turnover[1]]

a1 <- quantile(variables[, fee], 0.99, na.rm = TRUE)
a2 <- quantile(variables[, fee], 0.01, na.rm = TRUE)
variables <- variables[order(fee)
	][, extremum := ifelse(fee > a1, 2, ifelse(fee < a2, 1, 0))
	][extremum == 1, fee := fee[.N]
	][extremum == 2, fee := fee[1]]

a1 <- quantile(variables[, flow], 0.99, na.rm = TRUE)
a2 <- quantile(variables[, flow], 0.01, na.rm = TRUE)
variables <- variables[order(flow)
	][, extremum := ifelse(flow > a1, 2, ifelse(flow < a2, 1, 0))
	][extremum == 1, flow := flow[.N]
	][extremum == 2, flow := flow[1]]


# CSR要滞后
variables <- variables[order(id, date)
	][, CSR1.1 := shift(CSR1, n = 1, fill = NA, type = 'lag'), keyby = .(id)
	][, .(id, date, category1, category2, CSR1, CSR2, CSR1.1, raw_return, alpha_capm, alpha_3, alpha_4, alpha_5, alpha_6, Rsquare, vol, numstock, size, age, flow, fee, turnover)
	][, fee := as.numeric(fee)
	][, size := log(size)]


data <- variables[!is.na(CSR1.1)
	][, CSR1g := ntile(CSR1.1, 5), keyby = .(date)
	][, lapply(.SD[, 5:20], mean, na.rm = TRUE), keyby = .(date, CSR1g)]

# mean
a <- data[, lapply(.SD[, 2:17], mean, na.rm = TRUE), keyby = .(CSR1g)]

write.csv(a, "C://Users//shenfan//Desktop//aa.csv")

# dif
b = melt(data, id.vars = 1:2, measure.vars = 3:18)

b <- b[, value.2 := shift(value, n = 4, fill = NA, type = "lead"), keyby = .(variable, date)
	][CSR1g == 1
	][, dif := value.2 - value
	][, .(difference = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable)]

write.csv(b, "C://Users//shenfan//Desktop//aa.csv")


# regression
felm(CSR1.1 ~ alpha_3 + vol + numstock + size + age + fee + turnover | date + category2, variables) %>% summary()





plm(CSR1.1 ~ alpha_3 + vol + numstock + size + age + fee + turnover, variables, model = "pooling", effect = "twoways", index = c("id", "date")) %>% summary()

plm(CSR1.1 ~ alpha_3 + vol + numstock + size + age + fee + turnover, variables, model = "within", effect = "twoways", index = c("id", "date")) %>% summary()



stargazer(liner1, liner2, liner3, liner4, type = "html", out = "C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/change_hs300.doc", add.lines = list(c("stock", "yes", "yes", "yes", "yes"), c("date", "yes", "yes", "yes", "yes")), report = ('vc*t'))

