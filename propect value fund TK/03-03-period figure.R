load("cumret.RData")
load("monthlydata.RData")
data <- cum.ret[fund.TK, on = .(id, year, month)]

data <- data[order(id, year, month)
	][!is.nan(TK)
	][, TK.g := ntile(TK, 5), keyby = .(date)
	][, .(id, year, month, date, month_return.1, cum2.1, cum3.1, cum4.1, cum5.1, cum6.1, cum7.1, cum8.1, cum9.1, TK.g)]

data <- data[, cum1.1 := month_return.1 / 1
	][, cum2.1 := cum2.1 / 2
	][, cum3.1 := cum3.1 / 3
	][, cum4.1 := cum4.1 / 4
	][, cum5.1 := cum5.1 / 5
	][, cum6.1 := cum6.1 / 6
	][, cum7.1 := cum7.1 / 7
	][, cum8.1 := cum8.1 / 8
	][, cum9.1 := cum9.1 / 9]

data <- data[, .(id, year, month, date, cum1.1, cum2.1, cum3.1, cum4.1, cum5.1, cum6.1, cum7.1, cum8.1, cum9.1, TK.g)]

data <- melt(data, id = c("id", "year", "month", "date", "TK.g"), measures = c("cum1.1", "cum2.1", "cum3.1", "cum4.1", "cum5.1", "cum6.1", "cum7.1", "cum8.1", "cum9.1"))

TK.g <- data[, .(ret = mean(value)), keyby = .(variable, date, TK.g)
	][!is.na(ret)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(variable, date)
	][, dif := i.ret - ret]

a <- dif[, .(dif = mean(dif)), keyby = .(variable)
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



TK.g <- data[, .(ret = mean(cum8.1)), keyby = .(date, TK.g)
	][!is.na(ret)]

low <- TK.g[TK.g == 1]
high <- TK.g[TK.g == 5]
dif <- low[high, on = .(date)
	][, dif := i.ret - ret]

t.test(TK.g[TK.g == 5, ret])
t.test(dif[, dif])


t.test(dif[, dif])$p.value
t.test(dif[, dif])$statistic


fit <- lm(dif ~ 1, dif)
coeftest(fit, vcov. = NeweyWest)
