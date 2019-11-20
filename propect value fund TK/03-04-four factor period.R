load("fouralpha.RData")

# 滞后一期
alpha <- alpha[, alpha.1 := shift(four_alpha, n = 1, fill = NA, type = "lead"), keyby = .(id)]

# 滞后两期
for (j in 1:11) { 
ret <- alpha

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)]

reg.roll <- list()
for (i in (j+1):168) {
	reg.roll[[i]] <- ret[tag >= i - j & tag <= i, {
		j <- mean(alpha.1)
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

alpha <- reg.cof[ret, on = .(tag, id)]

}

setnames(alpha, 3:13, c("alpha.12", "alpha.11", "alpha.10", "alpha.9", "alpha.8", "alpha.7", "alpha.6", "alpha.5", "alpha.4", "alpha.3", "alpha.2"))

alpha <- alpha[, .(id, year, month, four_alpha, alpha.1, alpha.2, alpha.3, alpha.4, alpha.5, alpha.6, alpha.7, alpha.8, alpha.9, alpha.10, alpha.11, alpha.12)]


alpha <- alpha[, alpha.2 := shift(alpha.2, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.3 := shift(alpha.3, n = 2, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.4 := shift(alpha.4, n = 3, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.5 := shift(alpha.5, n = 4, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.6 := shift(alpha.6, n = 5, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.7 := shift(alpha.7, n = 6, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.8 := shift(alpha.8, n = 7, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.9 := shift(alpha.9, n = 8, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.10 := shift(alpha.10, n = 9, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.11 := shift(alpha.11, n = 10, fill = NA, type = "lead"), keyby = .(id)
	][, alpha.12 := shift(alpha.12, n = 11, fill = NA, type = "lead"), keyby = .(id)]

save(alpha, file = "alpha12period.RData")


#############################################################

for (i in 1:11) {
	a <- a[, colnames(alpha)[(i + 5L)] := shift(colnames(alpha)[(i + 5L)], n = i, fill = NA, type = "lead"), keyby = .(id)]

}


a <- a[, colnames(a[, i + 8]) := 1]


alpha[, {
	for (i in 1:11) {
	colnames(alpha)[(i + 4)] = shift(.SD[, (i + 3)], n = i, fill = NA, type = "lead")
	}
}
, keyby = .(id)
]