# multivariate analysis
# fund
load("variables.RData")

variables <- variables[order(id, date)
	][, .(id, date, category1, category2, CSR1, CSR2, raw_return, alpha_capm, alpha_3, alpha_4, alpha_5, alpha_6, Rsquare, vol, numstock, age, size, flow, fee, turnover)]

# Winsorize
variables <- variables[, colnames(variables[, 17:20]) := lapply(.SD[, 17:20], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)]

# ÖÍºó
variables <- variables[, CSR1.1 := shift(CSR1, n = 1, fill = NA, type = 'lag'), keyby = .(id)
	][, size := log(size)
	][, year := year(date)
	][, numstock := log(numstock)
	][, alpha_4 := alpha_4 * 100]

# reg
reg1 <- lm(CSR1.1 ~ raw_return + factor(year) + factor(category2), variables)
reg2 <- lm(CSR1.1 ~ alpha_4 + factor(year) + factor(category2), variables)
reg3 <- lm(CSR1.1 ~ flow + factor(year) + factor(category2), variables)
reg4 <- lm(CSR1.1 ~ raw_return + flow + vol + Rsquare + size + numstock + fee + turnover + age + factor(year) + factor(category2), variables)
reg5 <- lm(CSR1.1 ~ alpha_4 + flow + vol + Rsquare + size + numstock + fee + turnover + age + factor(year) + factor(category2), variables)

stargazer(reg1, reg2, reg3, reg4, reg5, type = "html", out = "C:/Users/shenfan/Desktop/csr/table/table5.doc", report = ('vc*t'))


###############################################################
# firm
load("variablesfirm.RData")

variable <- variable[, SOE := substring(EquityNatureID, 1, 1)
	][, SOE := ifelse(SOE == 1, 1, 0)]

# É¾³ýCSRÎªNA
variable <- variable[order(id, date)
	][, .(id, date, CSR, SOE, cash, FCF, CapEx, DSRatio, advertising, leverage, asset, BM, ROA, ROE, RD, CFOasset, ATO, efficiency)]

# Winsorize
variable <- variable[, colnames(variable[, 5:18]) := lapply(.SD[, 5:18], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)
	][, asset := log(asset)
	][, cash := log(cash)
	][, advertising := log(advertising)
	][, CapEx := log(CapEx)
	][!is.na(CSR)]  











