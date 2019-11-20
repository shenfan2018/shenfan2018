# table2
load("variablesfirm.RData")

variables <- variable[, SOE := substring(EquityNatureID, 1, 1)
	][, SOE := ifelse(SOE == 1, 1, 0)]

# É¾³ýCSRÎªNA
variables <- variables[order(id, date)
	][, .(id, date, CSR, SOE, cash, FCF, CapEx, DSRatio, advertising, leverage, asset, BM, ROA, ROE, RD, CFOasset, ATO, efficiency)]

# Winsorize
variables <- variables[, colnames(variables[, 5:18]):= lapply(.SD[, 5:18], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)
	][, asset := log(asset)
	][, cash := log(cash)
	][, advertising := log(advertising)
	][, CapEx := log(CapEx)
	][!is.na(CSR)]

data <- variables[, CSRg := ntile(CSR, 5), keyby = .(date)
	][, lapply(.SD[, 3:17], mean, na.rm = TRUE), keyby = .(date, CSRg)]

# mean
a <- data[, lapply(.SD[, 2:16], mean, na.rm = TRUE), keyby = .(CSRg)]

write.csv(a, "C://Users//shenfan//Desktop//aa.csv")

# dif
b = melt(data, id.vars = 1:2, measure.vars = 3:17)

b <- b[, value.2 := shift(value, n = 4, fill = NA, type = "lead"), keyby = .(variable, date)
	][CSRg == 1
	][, dif := value.2 - value
	][, .(difference = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable)]

write.csv(b, "C://Users//shenfan//Desktop//aa.csv")



####################################################################
# CSR dummy
load("variablesfirm.RData")

variables <- variable[, SOE := substring(EquityNatureID, 1, 1)
	][, SOE := ifelse(SOE == 1, 1, 0)]

variables <- variables[order(id, date)
	][, .(id, date, CSR, SOE, cash, FCF, CapEx, DSRatio, advertising, leverage, asset, BM, ROA, ROE, RD, CFOasset, ATO, efficiency)]

# Winsorize
variables <- variables[, colnames(variables[, 5:18]) := lapply(.SD[, 5:18], Winsorize, probs = c(0.01, 0.99), na.rm = TRUE)
	][, asset := log(asset)
	][, cash := log(cash)
	][, advertising := log(advertising)
	][, CapEx := log(CapEx)]

# NA ±ä³Édummy
variables <- variables[, CSRdummy := ifelse(is.na(CSR), 0, 1)]

data <- variables[, lapply(.SD[, 3:17], mean, na.rm = TRUE), keyby = .(date, CSRdummy)]

# mean
a <- data[, lapply(.SD[, 2:16], mean, na.rm = TRUE), keyby = .(CSRdummy)]

write.csv(a, "C://Users//shenfan//Desktop//aa.csv")

# dif
b = melt(data, id.vars = 1:2, measure.vars = 3:17)

b <- b[, value.2 := shift(value, n = 1, fill = NA, type = "lead"), keyby = .(variable, date)
	][CSRdummy == 0
	][, dif := value.2 - value
	][, .(difference = t.test(dif)$estimate, t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(variable)]

write.csv(b, "C://Users//shenfan//Desktop//aa.csv")


