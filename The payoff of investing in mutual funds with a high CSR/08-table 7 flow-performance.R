# flow-performance
load("variables.RData")

data <- variables[order(id, date)
	][, .(id, date, category1, category2, CSR1, CSR2, raw_return, alpha_capm, alpha_3, alpha_4, alpha_5, alpha_6, Rsquare, vol, numstock, age, size, flow, fee, turnover)]

# Winsorize
data <- data[, colnames(data[, 17:20]) := lapply(.SD[, 17:20], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# ÖÍºó
data <- data[, CSR1.1 := shift(CSR1, n = 1, fill = NA, type = 'lag'), keyby = .(id)
	][, flow.1 := shift(flow, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, size := log(size)
	][, year := year(date)
	][, numstock := log(numstock)
	][, alpha_5 := alpha_5 * 100]

reg1 <- lm(flow.1 ~ alpha_5 + factor(year) + factor(category2), data)

reg2 <- lm(flow.1 ~ alpha_5 * CSR1.1 + factor(year) + factor(category2), data)

reg3 <- lm(flow.1 ~ alpha_5 * CSR1.1 + flow + vol + Rsquare + size + numstock + fee + turnover + age + factor(year) + factor(category2), data)

stargazer(reg1, reg2, reg3, type = "html", out = "C:/Users/shenfan/Desktop/csr/table/table7.doc", report = ('vc*t'))
