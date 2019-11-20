# return 的各期
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
data.m <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)
	][, month_return.1 := shift(month_return, n = 1, fill = NA, type = "lead"), keyby = .(id)]

# 2
ret <- data.m

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)
	][, month_return2 := month_return.1 + 1]

reg.roll <- list()
for (i in 2:168) {
	reg.roll[[i]] <- ret[tag >= i - 1 & tag <= i, {
		I <- prod(month_return2) - 1
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

data.m <- reg.cof[ret, on = .(tag, id)]

setnames(data.m, "V1", "cum2")

# 3
ret <- data.m

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)
	][, month_return2 := month_return.1 + 1]

reg.roll <- list()
for (i in 3:168) {
	reg.roll[[i]] <- ret[tag >= i - 2 & tag <= i, {
		I <- prod(month_return2) - 1
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

data.m <- reg.cof[ret, on = .(tag, id)]

setnames(data.m, "V1", "cum3")

# 4
ret <- data.m

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)
	][, month_return2 := month_return.1 + 1]

reg.roll <- list()
for (i in 4:168) {
	reg.roll[[i]] <- ret[tag >= i - 3 & tag <= i, {
		I <- prod(month_return2) - 1
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

data.m <- reg.cof[ret, on = .(tag, id)]

setnames(data.m, "V1", "cum4")

# 5
ret <- data.m

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)
	][, month_return2 := month_return.1 + 1]

reg.roll <- list()
for (i in 5:168) {
	reg.roll[[i]] <- ret[tag >= i - 4 & tag <= i, {
		I <- prod(month_return2) - 1
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

data.m <- reg.cof[ret, on = .(tag, id)]

setnames(data.m, "V1", "cum5")

# 6
ret <- data.m

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)
	][, month_return2 := month_return.1 + 1]

reg.roll <- list()
for (i in 6:168) {
	reg.roll[[i]] <- ret[tag >= i - 5 & tag <= i, {
		I <- prod(month_return2) - 1
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

data.m <- reg.cof[ret, on = .(tag, id)]

setnames(data.m, "V1", "cum6")

# 7
ret <- data.m

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)
	][, month_return2 := month_return.1 + 1]

reg.roll <- list()
for (i in 7:168) {
	reg.roll[[i]] <- ret[tag >= i - 6 & tag <= i, {
		I <- prod(month_return2) - 1
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

data.m <- reg.cof[ret, on = .(tag, id)]

setnames(data.m, "V1", "cum7")

# 8
ret <- data.m

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)
	][, month_return2 := month_return.1 + 1]

reg.roll <- list()
for (i in 8:168) {
	reg.roll[[i]] <- ret[tag >= i - 7 & tag <= i, {
		I <- prod(month_return2) - 1
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

data.m <- reg.cof[ret, on = .(tag, id)]

setnames(data.m, "V1", "cum8")

# 9
ret <- data.m

ret <- ret[order(id, year, month)
	][, tag := seq(1:(.N)), keyby = .(id)
	][, month_return2 := month_return.1 + 1]

reg.roll <- list()
for (i in 9:168) {
	reg.roll[[i]] <- ret[tag >= i - 8 & tag <= i, {
		I <- prod(month_return2) - 1
	},
	keyby = .(id)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[1:168]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

data.m <- reg.cof[ret, on = .(tag, id)]

setnames(data.m, "V1", "cum9")

cum.ret <- data.m[, .(id, year, month, month_return, month_return.1, cum2, cum3, cum4, cum5, cum6, cum7, cum8, cum9)]

cum.ret <- cum.ret[, cum2.1 := shift(cum2, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, cum3.1 := shift(cum3, n = 2, fill = NA, type = "lead"), keyby = .(id)
	][, cum4.1 := shift(cum4, n = 3, fill = NA, type = "lead"), keyby = .(id)
	][, cum5.1 := shift(cum5, n = 4, fill = NA, type = "lead"), keyby = .(id)
	][, cum6.1 := shift(cum6, n = 5, fill = NA, type = "lead"), keyby = .(id)
	][, cum7.1 := shift(cum7, n = 6, fill = NA, type = "lead"), keyby = .(id)
	][, cum8.1 := shift(cum8, n = 7, fill = NA, type = "lead"), keyby = .(id)
	][, cum9.1 := shift(cum9, n = 8, fill = NA, type = "lead"), keyby = .(id)]

save(cum.ret,file = "cumret.RData")