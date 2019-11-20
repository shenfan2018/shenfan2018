# CSR and firm characterstic
load("variablesfirm.RData")

variables <- variable[, SOE := substring(EquityNatureID, 1, 1)
	][, SOE := ifelse(SOE == 1, 1, 0)]

# É¾³ýCSRÎªNA
variables <- variables[order(id, date)
	][, .(id, date, CSR, SOE, cash, FCF, CapEx, DSRatio, advertising, leverage, asset, BM, ROA, ROE, RD, CFOasset, ATO, efficiency)]

# Winsorize
variables <- variables[, colnames(variables[, 5:18]) := lapply(.SD[, 5:18], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)
	][, asset := log(asset)
	][, cash := log(cash)
	][, advertising := log(advertising)
	][, CapEx := log(CapEx)
	][!is.na(CSR)
	][, year := year(date)]

# reg
reg1 <- lm(CSR ~ SOE + factor(id) + factor(year), variables)

reg2 <- lm(CSR ~ efficiency + factor(id) + factor(year), variables)

reg3 <- lm(CSR ~ SOE + cash + FCF + CapEx + DSRatio + advertising + leverage + asset + BM + ROA + factor(id) + factor(year), variables)

reg4 <- lm(CSR ~ efficiency + cash + FCF + CapEx + DSRatio + advertising + leverage + asset + BM + ROA + factor(id) + factor(year), variables)

stargazer(reg1, reg2, reg3, reg4, type = "html", out = "C:/Users/shenfan/Desktop/csr/table/table9.doc", report = ('vc*t'))
