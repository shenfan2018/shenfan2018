load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 半年的收益率 (5是收益最好的）
Fund.s <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(sem_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, sem)
	][order(year, sem)
	][, top20 := ntile(sem_return, 5), keyby = .(year, sem)] # 改


load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-portfolio.RData")
portfolio <- portfolio[, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2]


# 读取MA和Q
MA <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/ma_result.csv")

# 这是一种将1变成000001的方式
# a <- sprintf("%06d", 1:100)
MA <- MA[, stock.id := sprintf("%06d", id)
	][, .(stock.id, year, efficiency, manager_ability)]

#匹配
a <- MA[portfolio, on = .(stock.id, year), nomatch = 0
	][order(id, year, sem)
	][, .(E_score = sum(proportion.stock * efficiency, na.rm = TRUE), Q_score = sum(proportion.stock * manager_ability, na.rm = TRUE)), keyby = .(id, year, sem)]

# 筛选top20的基金
a <- Fund.s[a, on = .(id, year, sem)
	][order(year, sem)
	][, E_ntile := ntile(E_score, 5), keyby = .(year, sem)
	][, Q_ntile := ntile(Q_score, 5), keyby = .(year, sem)
	][year < 2017]

a <- a[order(id, year, sem)]

a <- a[, Et0 := ifelse(E_ntile == 5 & top20 == 5, 1, 0)
	][, Qt0 := ifelse(Q_ntile == 5 & top20 == 5, 1, 0)
	][top20 == 5, Dt0 := ifelse(E_ntile == 5 | Q_ntile == 5, 0, 1)] ## 改


# 所有基金的月度收益
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
Fund.m <- data.NAV[, year := year(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, month)
	][month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, sem := 1
	][month == 7 | month == 8 | month == 9 | month == 10 | month == 11 | month == 12, sem := 2
	][month == 1 | month == 2 | month == 3, quarter := 1
	][month == 4 | month == 5 | month == 6, quarter := 2
	][month == 7 | month == 8 | month == 9, quarter := 3
	][month == 10 | month == 11 | month == 12, quarter := 4]


Fund.R2 <- a[Fund.m, on = .(id, year, sem), nomatch = 0
	][order(id, year, month)]

Fund.R2 <- Fund.R2[, .(id, year, month, quarter, sem, top20, E_ntile, Q_ntile, Et0, Qt0, Dt0, month_return)]

SJCS <- Fund.R2[, .(id, year, quarter, Et0)
	][, yesno := duplicated(Et0), keyby = .(id, year, quarter)
	][yesno == FALSE, .(id, year, quarter, Et0)
	][, Et1 := shift(Et0, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Et2 := shift(Et1, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Et3 := shift(Et2, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Et4 := shift(Et3, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Et5 := shift(Et4, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Et6 := shift(Et5, n = 2, fill = NA, type = "lag"), keyby = .(id)]

Fund.R2 <- SJCS[Fund.R2, on = .(id, year, quarter)]

# Q
SJCS <- Fund.R2[, .(id, year, quarter, Qt0)
	][, yesno := duplicated(Qt0), keyby = .(id, year, quarter)
	][yesno == FALSE, .(id, year, quarter, Qt0)
	][, Qt1 := shift(Qt0, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Qt2 := shift(Qt1, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Qt3 := shift(Qt2, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Qt4 := shift(Qt3, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Qt5 := shift(Qt4, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Qt6 := shift(Qt5, n = 2, fill = NA, type = "lag"), keyby = .(id)]

Fund.R2 <- SJCS[Fund.R2, on = .(id, year, quarter)]

# D
SJCS <- Fund.R2[, .(id, year, quarter, Dt0)
	][, yesno := duplicated(Dt0), keyby = .(id, year, quarter)
	][yesno == FALSE, .(id, year, quarter, Dt0)
	][, Dt1 := shift(Dt0, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Dt2 := shift(Dt1, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Dt3 := shift(Dt2, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Dt4 := shift(Dt3, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Dt5 := shift(Dt4, n = 2, fill = NA, type = "lag"), keyby = .(id)
	][, Dt6 := shift(Dt5, n = 2, fill = NA, type = "lag"), keyby = .(id)]

Fund.R2 <- SJCS[Fund.R2, on = .(id, year, quarter)]

## ET
Fund.R2 <- Fund.R2[order(year, month, id)]

Fund.Q <- Fund.R2[Et0 == 1, .(retE0 = mean(month_return, na.rm = TRUE)), keyby = .(year, month)]

Et <- Fund.R2[Et1 == 1, .(retE1 = mean(month_return, na.rm = TRUE)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Et2 == 1, .(retE2 = mean(month_return, na.rm = TRUE)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Et3 == 1, .(retE3 = mean(month_return, na.rm = TRUE)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Et4 == 1, .(retE4 = mean(month_return, na.rm = TRUE)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Et5 == 1, .(retE5 = mean(month_return, na.rm = TRUE)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Et6 == 1, .(retE6 = mean(month_return, na.rm = TRUE)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

# Qt
Et <- Fund.R2[Qt0 == 1, .(retQ0 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Qt1 == 1, .(retQ1 = mean(month_return, na.rm = TRUE)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Qt2 == 1, .(retQ2 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Qt3 == 1, .(retQ3 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Qt4 == 1, .(retQ4 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Qt5 == 1, .(retQ5 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Qt6 == 1, .(retQ6 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

# Dt
Et <- Fund.R2[Dt0 == 1, .(retD0 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Dt1 == 1, .(retD1 = mean(month_return, na.rm = TRUE)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Dt2 == 1, .(retD2 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Dt3 == 1, .(retD3 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Dt4 == 1, .(retD4 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Dt5 == 1, .(retD5 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

Et <- Fund.R2[Dt6 == 1, .(retD6 = mean(month_return)), keyby = .(year, month)]

Fund.Q <- Et[Fund.Q, on = .(year, month)]

###
sf = melt(Fund.Q, id.vars = c("year", "month"), measure.vars = c("retE0", "retE1", "retE2", "retE3", "retE4", "retE5", "retE6", "retQ0", "retQ1", "retQ2", "retQ3", "retQ4", "retQ5", "retQ6", "retD0", "retD1", "retD2", "retD3", "retD4", "retD5", "retD6"))

setnames(sf, c("value", "variable"), c("ret", "dif"))


# 计算所有股票的capm等
# 各种因子以及alpha
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

syf <- four[sf, on = .(year, month)]

syf <- syf[, rit := ret - rf
	][, .(alpha_capm = coef(lm(rit ~ mkt_rf))[1], alpha_3 = coef(lm(rit ~ mkt_rf + smb + hml))[1], alpha_4 = coef(lm(rit ~ mkt_rf + smb + hml + umd))[1], alpha_5 = coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma))[1]), keyby = .(dif)]

syf <- syf[, period := seq(from = 0, to = 6)
	][, group := substring(dif, 4, 4)]

syf <- syf[period > 0
	][group == "Q", group := "Manager_ability"
	][group == "E", group := "Efficiency"
	][group == "D", group := "treat"]


ggplot(syf, aes(x = period)) +
	geom_line(aes(y = alpha_capm, linetype = group))

ggplot(syf, aes(x = period)) +
	geom_line(aes(y = alpha_3, linetype = group))

ggplot(syf, aes(x = period)) +
	geom_line(aes(y = alpha_5, linetype = group))