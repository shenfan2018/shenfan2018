load('sample.RData')

t.test(stock.p[num.name == 3, "ret"])
t.test(stock.p[num.name == 4, "ret"])
t.test(stock.p[num.name == 3, "ret"], stock.p[num.name == 4, "ret"], paired = FALSE)

# 各种因子以及alpha 
# http://sf.cufe.edu.cn/kxyj/kyjg/zgzcglyjzx/zlxzzq/111422.htm
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

# match 牛为1，熊为0
stock.p <- four[stock.p, on = .(year, month)]
stock.p <- stock.p[, mktrf := ifelse(mkt_rf > 0, 1, 0)
	][, riskpre := ret - rf]

# 牛
t.test(stock.p[num.name == 3 & mktrf == 1, "ret"])
t.test(stock.p[num.name == 4 & mktrf == 1, "ret"])
t.test(stock.p[num.name == 3 & mktrf == 1, "ret"], stock.p[num.name == 4 & mktrf == 1, "ret"], paired = FALSE)

# 熊
t.test(stock.p[num.name == 3 & mktrf == 0, "ret"])
t.test(stock.p[num.name == 4 & mktrf == 0, "ret"])
t.test(stock.p[num.name == 3 & mktrf == 0, "ret"], stock.p[num.name == 4 & mktrf == 0, "ret"], paired = FALSE)

## risk preimum
t.test(stock.p[num.name == 3, "riskpre"])
t.test(stock.p[num.name == 4, "riskpre"])
t.test(stock.p[num.name == 3, "riskpre"], stock.p[num.name == 4, "riskpre"], paired = FALSE)

# 牛
t.test(stock.p[num.name == 3 & mktrf == 1, "riskpre"])
t.test(stock.p[num.name == 4 & mktrf == 1, "riskpre"])
t.test(stock.p[num.name == 3 & mktrf == 1, "riskpre"], stock.p[num.name ==4 & mktrf == 1, "riskpre"], paired = FALSE)

# 熊
t.test(stock.p[num.name == 3 & mktrf == 0, "riskpre"])
t.test(stock.p[num.name == 4 & mktrf == 0, "riskpre"])
t.test(stock.p[num.name == 3 & mktrf == 0, "riskpre"], stock.p[num.name == 4 & mktrf == 0, "riskpre"], paired = FALSE)


## equal weighted 总体
eq <- stock.p[, .(eq_ret = mean(ret, na.rm = TRUE)), keyby = .(year, month, num.name)]
eq <- four[eq, on = .(year, month), nomatch = 0
	][, rit := eq_ret - rf]


lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, eq[num.name == 3]) %>% summary()
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, eq[num.name == 4]) %>% summary()

# gmm 广义矩估计
# pgmm(rit ~ mkt_rf + smb + hml + rmw + cma, data = eq[num.name == 3], effect = "individual", model = "onestep") %>% summary()

# difference
dif.eq <- eq[, zhi := shift(eq_ret, n = 1, fill = NA, type = "lead")
	][num.name == 3
	][, dif := eq_ret - zhi]

lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, dif.eq) %>% summary()

# 调试p.value
# a <- lm(dif ~ mkt_rf + smb + hml + rmw + cma, dif.eq) %>% summary() %>% list() %>% lapply(`[[`, "coefficients") %>% lapply(`[`, 19) %>% unlist()
# lm(dif ~ mkt_rf + smb + hml + rmw + cma, dif.eq) %>% summary()

# 分行业的
eq.hy <- stock.p[, .(eq_ret = mean(ret, na.rm = TRUE)), keyby = .(year, month, hy, num.name)]
eq.hy <- four[eq.hy, on = .(year, month), nomatch = 0
	][, rit := eq_ret - rf]
# 删除该阶段没有，选出有对比的jieduan==2
eq.hy <- eq.hy[, jieduan := .N, keyby = .(year, month, hy)
	][jieduan == 2
	][, zhi := shift(eq_ret, n = 1, fill = NA, type = "lead")
	][num.name == 3
	][, dif := eq_ret - zhi
	][, .(coef = coef(lm(dif ~ mkt_rf + smb + hml + rmw + cma))[1], p.value = lm(dif ~ mkt_rf + smb + hml + rmw + cma) %>% summary() %>% list() %>% lapply(`[[`, "coefficients") %>% lapply(`[`, 19) %>% unlist(), num = (.N)), keyby = .(hy)]

## value weighted
# 这里要加入月末的流通股市值(千元）
ltsz <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//月个股流通市值//TRD_Mnth.xlsx")
ltsz <- as.data.table(ltsz)
setnames(ltsz, 1:3, c("stock.id", "year", "month"))
vw <- ltsz[stock.p, on = .(stock.id, year, month), nomatch = 0
	][, sum_sz := sum(Msmvosd, na.rm = TRUE), keyby = .(year, month, num.name)
	][, pro := Msmvosd / sum_sz
	][, .(vw_ret = sum(ret * pro, na.rm = TRUE)), keyby = .(year, month, num.name)]

vw <- four[vw, on = .(year, month), nomatch = 0
	][, rit := vw_ret - rf]

lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, vw[num.name == 3]) %>% summary()
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, vw[num.name == 4]) %>% summary()

# difference
dif.vw <- vw[, zhi := shift(vw_ret, n = 1, fill = NA, type = "lead")
	][num.name == 3
	][, dif := vw_ret - zhi]

lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, dif.vw) %>% summary()

################################################################################# 
# new 分牛熊
load('sample.RData')

# 各种因子以及alpha 
# http://sf.cufe.edu.cn/kxyj/kyjg/zgzcglyjzx/zlxzzq/111422.htm
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

# match 牛为1，熊为0
stock.p <- four[stock.p, on = .(year, month)]
stock.p <- stock.p[, mktrf := ifelse(mkt_rf > 0, 1, 0)
	][, riskpre := ret - rf]

## equal weighted 总体
eq <- stock.p[, .(eq_ret = mean(ret, na.rm = TRUE)), keyby = .(year, month, num.name)]
eq <- four[eq, on = .(year, month), nomatch = 0
	][, rit := eq_ret - rf]

# all
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, eq[num.name == 3]) %>% summary()
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, eq[num.name == 4]) %>% summary()

# difference
dif.eq <- eq[, zhi := shift(eq_ret, n = 1, fill = NA, type = "lead")
	][num.name == 3
	][, dif := eq_ret - zhi]

lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, dif.eq) %>% summary()



#####################################################################################
load('sample.RData')
stock.p <- stock.p[, .(stock.id, year, month, num.name, ret)
	][, .(ret = mean(ret)), keyby = .(year, month, num.name)]

# 各种因子以及alpha 
# http://sf.cufe.edu.cn/kxyj/kyjg/zgzcglyjzx/zlxzzq/111422.htm
four <- fread("C:/Users/shenfan/Desktop/one word makes abnormal profit/fama monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
# match 牛为1，熊为0
stock.p <- four[stock.p, on = .(year, month)]
stock.p <- stock.p[, mktrf := ifelse(mkt_rf > 0, 1, 0)
	][, riskpre := ret - rf]

dif <- stock.p[, ret4 := shift(ret, n = 1, fill = NA, type = "lead"), keyby = .(year, month)
	][num.name == 3
	][, dif := ret - ret4]

#



# all sample
all3 <- lm(riskpre ~ mkt_rf + smb + hml + rmw + cma + umd, stock.p[num.name == 3 ])
all4 <- lm(riskpre ~ mkt_rf + smb + hml + rmw + cma + umd, stock.p[num.name == 4 ])
alldif <- lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, dif[])

stargazer(all3, all4, alldif, type = "html", out = "C://Users//shenfan//Desktop//one word makes abnormal profit//table//factorall.doc")

# 牛
bull3 <- lm(riskpre ~ mkt_rf + smb + hml + rmw + cma + umd, stock.p[num.name == 3 & mktrf == 1])
bull4 <- lm(riskpre ~ mkt_rf + smb + hml + rmw + cma + umd, stock.p[num.name == 4 & mktrf == 1])
bulldif <- lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, dif[mktrf == 1])

stargazer(bull3, bull4, bulldif, type = "html", out = "C://Users//shenfan//Desktop//one word makes abnormal profit//table//factorbull.doc")

# 熊
bear3 <- lm(riskpre ~ mkt_rf + smb + hml + rmw + cma + umd, stock.p[num.name == 3 & mktrf == 0])
bear4 <- lm(riskpre ~ mkt_rf + smb + hml + rmw + cma + umd, stock.p[num.name == 4 & mktrf == 0])
beardif <- lm(dif ~ mkt_rf + smb + hml + rmw + cma + umd, dif[mktrf == 0])

stargazer(bear3, bear4, beardif, type = "html", out = "C://Users//shenfan//Desktop//one word makes abnormal profit//table//factorbear.doc")