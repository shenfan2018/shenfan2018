load("fouralpha.RData")
setnames(reg.cof, 3, "four_alpha")

load("monthlydata.RData")

fund.TK <- fund.TK[, .(id, year, month, TK, month_return)
	][, tag := seq(1:.N), keyby = .(id)]

data <- reg.cof[fund.TK, on = .(id, tag)
	][!is.nan(TK)
	][, .(id, year, month, TK, month_return, four_alpha)
	][, TK.g := ntile(TK, 5), keyby = .(year, month)]

# 
write.csv(data, "C://Users//shenfan//Desktop//persistence.csv")

################################################################################
# 1-12 month
data <- data[order(id, year, month)]
for (i in 1:12) {
	data[, str_c("alpha.", 1:12)[i] := shift(four_alpha, n = i, fill = NA, type = "lead")
	, keyby = .(id)]
}

data = melt(data, id.vars = c("id", "year", "month", "TK", "TK.g", "four_alpha", "month_return"), measures.vars = colnames(data[, 8:19]))

# alpha mean
alpha <- data[, .(alpha = mean(value, na.rm = TRUE)), keyby = .(year, month, TK.g, variable)]

low <- alpha[TK.g == 1]
high <- alpha[TK.g == 5]
dif <- low[high, on = .(year, month, variable)
	][, dif := i.alpha - alpha
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate), keyby = .(variable)
	][, period := 1:(.N)]

ggplot(dif, aes(x = period)) +
	geom_line(aes(y = ret)) +
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


###################################################################
# 1,3,6,9,12
data <- data[, alpha1 := alpha.1
	][, alpha3 := (alpha.1 + alpha.2 + alpha.3)/3
	][, alpha6 := (alpha3 * 3 + alpha.4 + alpha.5 + alpha.6)/6
	][, alpha9 := (alpha6 * 6 + alpha.7 + alpha.8 + alpha.9)/9
	][, alpha12 := (alpha9 * 9 + alpha.10 + alpha.11 + alpha.12)/12
	][, .(id, year, month, TK, TK.g, alpha1, alpha6, alpha9, alpha12)]

# another
data <- data[, alpha1 := alpha.1
	][, alpha3 := (alpha.1 + alpha.2 + alpha.3) / 3
	][, alpha6 := (alpha.4 + alpha.5 + alpha.6) / 3
	][, alpha9 := (alpha.7 + alpha.8 + alpha.9) / 3
	][, alpha12 := (alpha.10 + alpha.11 + alpha.12) / 3
	][, .(id, year, month, TK, TK.g, alpha1, alpha3, alpha6, alpha9, alpha12)]


data = melt(data, id.vars = c("id", "year", "month", "TK", "TK.g"), measures.vars = colnames(data[, 6:10]))

alpha <- data[!is.na(value)
	][, .(alpha = mean(value, na.rm = TRUE)), keyby = .(year, month, TK.g, variable)]

low <- alpha[TK.g == 1]
high <- alpha[TK.g == 5]
dif <- low[high, on = .(year, month, variable)
	][, dif := i.alpha - alpha
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate), keyby = .(variable)
	][, period := c("1", "3", "6", "9", "12")]

ggplot(dif, aes(x = period)) +
	geom_line(aes(y = ret)) +
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



load("monthlydata.RData")
fund.TK <- fund.TK[!is.nan(TK)
	][, .(id, year, month, TK, month_return)
	]

fund.TK <- data[order(id, year, month)
	][, four_alpha.1 := shift(four_alpha, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, TK.g := ntile(TK, 5), keyby = .(year, month)
	][, .(ret = mean(four_alpha.1, na.rm = TRUE)), keyby = .(year, month, TK.g)]

low <- fund.TK[TK.g == 1]
high <- fund.TK[TK.g == 5]
dif <- low[high, on = .(year, month)
	][, dif := i.ret - ret
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate)]



################################################################################raw return

load("fouralpha.RData")
setnames(reg.cof, 3, "four_alpha")

load("monthlydata.RData")

fund.TK <- fund.TK[, .(id, year, month, TK, month_return)
	][, tag := seq(1:.N), keyby = .(id)]

data <- reg.cof[fund.TK, on = .(id, tag)
	][!is.nan(TK)
	][, .(id, year, month, TK, month_return, four_alpha)
	][, TK.g := ntile(TK, 5), keyby = .(year, month)]

# write.csv(data, "C://Users//shenfan//Desktop//persistence.csv")

################################################################################
# 1-12 month
data <- data[order(id, year, month)]
for (i in 1:12) {
	data[, str_c("month_return.", 1:12)[i] := shift(month_return, n = i, fill = NA, type = "lead")
	, keyby = .(id)]
}

# 1,3,6,9,12
data <- data[, alpha1 := month_return.1
	][, alpha3 := (month_return.1 + month_return.2 + month_return.3) / 3
	][, alpha6 := (alpha3 * 3 + month_return.4 + month_return.5 + month_return.6) / 6
	][, alpha9 := (alpha6 * 6 + month_return.7 + month_return.8 + month_return.9) / 9
	][, alpha12 := (alpha9 * 9 + month_return.10 + month_return.11 + month_return.12) / 12
	][, .(id, year, month, TK, TK.g, alpha1, alpha3, alpha6, alpha9, alpha12)]

# another
data <- data[, alpha1 := month_return.1
	][, alpha3 := (month_return.1 + month_return.2 + month_return.3) / 3
	][, alpha6 := (month_return.4 + month_return.5 + month_return.6) / 3
	][, alpha9 := (month_return.7 + month_return.8 + month_return.9) / 3
	][, alpha12 := (month_return.10 + month_return.11 + month_return.12) / 3
	][, .(id, year, month, TK, TK.g, alpha1, alpha3, alpha6, alpha9, alpha12)]


data = melt(data, id.vars = c("id", "year", "month", "TK", "TK.g"), measures.vars = colnames(data[, 6:10]))

alpha <- data[!is.na(value)
	][, .(alpha = mean(value, na.rm = TRUE)), keyby = .(year, month, TK.g, variable)]

low <- alpha[TK.g == 1]
high <- alpha[TK.g == 5]
dif <- low[high, on = .(year, month, variable)
	][, dif := i.alpha - alpha
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], ret = t.test(dif)$estimate), keyby = .(variable)
	][, period := c(1, 3, 6, 9, 12)]

ggplot(dif, aes(x = period)) +
	geom_line(aes(y = ret)) 
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

write.csv(dif, "C://Users//shenfan//Desktop//figure.csv")



#######################################################################
## 先生成再索引
#a <- data
#a <- a[order(id, year, month)
	#][, .(id, year, month, four_alpha)]

#for (i in 1:10) {
	#a[, str_c("four_alpha", 1:10)[i] := shift(four_alpha, n = i, fill = NA, type = "lead")
	#, keyby = .(id)]
#}

#for (i in 1:10) {
	#a[, str_c("four_alpha", i) := shift(four_alpha, n = i, fill = NA, type = "lead")
	#, keyby = .(id)]
#}

#mtcars <- as.data.table(mtcars)
#b <- str_c(colnames(mtcars), 1:ncol(mtcars))
#for (i in 1:ncol(mtcars)) {
	#mtcars[, b[i] := shift(.SD, n = i, fill = NA, type = "lead"), .SDcols = i]
#}

#mtcars[, .SD[, 2]]
#mtcars[, .SD, .SDcols = 2]
#mtcars[, lapply(.SD, mean), .SDcols = 2:3, by = .(vs)]

#mtcars[2, .SD]
#mtcars[, .SD[2]]





