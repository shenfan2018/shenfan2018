load("alpha12period.RData")
load("monthlydata.RData")
data <- alpha[fund.TK, on = .(id, year, month)]

data <- data[order(id, year, month)
	][!is.nan(TK)
	][, TK.g := ntile(TK, 5), keyby = .(date)]

data <- data[, .(id, year, month, date, alpha.1, alpha.2, alpha.3, alpha.4, alpha.5, alpha.6, alpha.7, alpha.8, alpha.9, alpha.10, alpha.11, alpha.12, TK.g)]

data <- melt(data, id = c("id", "year", "month", "date", "TK.g"), measures = c("alpha.1", "alpha.2", "alpha.3", "alpha.4", "alpha.5", "alpha.6", "alpha.7", "alpha.8", "alpha.9", "alpha.10", "alpha.11", "alpha.12"))

# TK.g <- data[, .(ret = mean(value)), keyby = .(variable, date, TK.g)
#	][!is.na(ret)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(variable, date)
	][, dif := i.ret - ret]

a <- dif[, .(dif = mean(dif)), keyby = .(variable)
	][, period := seq(1:(.N))]

# a <- dif[, .(pvalue = t.test(dif)$p.value), keyby = .(variable)
#	][, period := seq(1:(.N))]

# a <- dif[, .(t = t.test(dif)$statistic), keyby = .(variable)
#	][, period := seq(1:(.N))]

a <- dif[, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3]), keyby = .(variable)
	][, period := seq(1:(.N))]


ggplot(a, aes(x = period)) +
	geom_line(aes(y = dif)) +
	theme_bw() +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 8),
#	legend.title = element_blank(),
	legend.position = c(0.15, 0.85),
)


t.test(dif[variable == "alpha.2", dif])

t.test(dif[variable=="alpha.2", dif])$p.value
t.test(dif[, dif])$statistic


fit <- lm(dif ~ 1, dif[variable == "alpha.2"]) 
coeftest(fit, vcov. = NeweyWest)

a <- lm(dif ~ 1, dif[variable == "alpha.2"]) %>% coeftest(vcov. = NeweyWest) $ coef[,3]

coeftest(lm(dif ~ 1, dif[variable == "alpha.2"]), vcov. = NeweyWest)[,3]
