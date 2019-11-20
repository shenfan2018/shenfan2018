# all sample stocks on the basis of changes in IO
time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")

## 基金持股比例
IOF <- read_excel("C://Users//shenfan//Desktop//社保//data//基金持股比例.xlsx")
IOF <- as.data.table(IOF)
setnames(IOF, 1:58, c("code", "name", time))
IOF = melt(IOF, id.vars = c("code", "name"), measure.vars = time)
setnames(IOF, c("value", "variable"), c("IO.Fund", "DateQ"))

IOF <- IOF[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][quarter == 2 | quarter == 4
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(stock.id, year, sem, IO.Fund)]

# 滞后
IOF <- IOF[, IO.Fund.2 := shift(IO.Fund, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, IO.Fund.change := IO.Fund - IO.Fund.2]
	
load("stock.RData")
stock.s <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, .SD[.N], keyby = .(stock.id, year, sem)
	][, .(stock.id, year, sem, Dsmvosd)]

# value 来分ntile ( 这里有问题 ） 有些股票7891011不在，12在，但是你还给了他比例
value <- stock.s[IOF, on = .(stock.id, year, sem)
	][, changes := Dsmvosd * IO.Fund.change
	][!is.na(changes)
	][, change.ntile := ntile(changes, 5), keyby = .(year, sem)
	][, value := IO.Fund * Dsmvosd
	][, proportion := value / sum(value), keyby = .(year, sem, change.ntile)
	][, .(stock.id, year, sem, change.ntile, proportion)]


load("stock.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, .(month_ret = prod(Dretwd2, na.rm = TRUE) - 1), keyby = .(stock.id, year, sem, month)]

## t+1 (一期为半年）
syf <- value[stock.m, on = .(stock.id, year, sem)
	][order(stock.id, year, month)
	][, change.ntile.2 := shift(change.ntile, n = 6, fill = NA, type = "lag"), keyby = .(stock.id)
	][, proportion.2 := shift(proportion, n = 6, fill = NA, type = "lag"), keyby = .(stock.id)
	][!is.na(proportion.2)
	][, .(ret = sum(proportion.2 * month_ret)), keyby = .(year, month, change.ntile.2)]


# a <- syf[!is.na(proportion)
#	][order(year, month, change.ntile)
#	][, SJ := sum(proportion), keyby = .(year, month, change.ntile)]


#
Q5 <- syf[change.ntile.2 == 5] #0.01496107  t = 2.0745, df = 155, p-value = 0.03969
t.test(Q5[, ret])
Q1 <- syf[change.ntile.2 == 1] #0.01515315  t = 2.0856, df = 155, p-value = 0.03865
t.test(Q1[, ret])
t.test(Q5[, ret], Q1[, ret], paired = TRUE) # t = -0.059321, df = 155, p-value = 0.9528

# regression
## 各种因子以及alpha
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

Q5 <- four[Q5, on = .(year, month), nomatch = 0
	][, rit := ret - rf]
fund.q5 <- lm(rit  ~ mkt_rf + smb + hml + rmw + cma + umd, Q5)# %>% summary() #(Intercept)  0.001400   0.002474   0.566  0.57239  

Q1 <- four[Q1, on = .(year, month), nomatch = 0
	][, rit := ret - rf]
fund.q1 <- lm(rit  ~ mkt_rf + smb + hml + rmw + cma + umd, Q1)# %>% summary() # (Intercept)  0.003521   0.001955   1.801  0.07371 . 

dif <- Q5[Q1, on = .(year, month)
	][, min := ret - i.ret] #ret为5，i.ret为1
fund.qd <- lm(min ~ mkt_rf + smb + hml + rmw + cma + umd, dif)# %>% summary() # (Intercept) -0.002121   0.003096  -0.685   0.4943



#####社保
## 社保基金持股比例
IOS <- read_excel("C://Users//shenfan//Desktop//社保//data//社保基金持股比例.xlsx")
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IO.social", "DateQ"))

IOS <- IOS[, DateQ := as.Date(as.character(DateQ))
	][, name := NULL
	][order(code, DateQ)
	][, year := year(DateQ)
	][, quarter := quarter(DateQ)
	][, stock.id := substring(code, 1, 6)
	][quarter == 2 | quarter == 4
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(stock.id, year, sem, IO.social)]

# 滞后
IOS <- IOS[, IO.social.2 := shift(IO.social, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, IO.social.change := IO.social - IO.social.2]

# value 来分ntile
value <- stock.s[IOS, on = .(stock.id, year, sem)
	][, changes := Dsmvosd * IO.social.change
	][!is.na(changes)
	][, change.ntile := ntile(changes, 5), keyby = .(year, sem)
	][, value := IO.social * Dsmvosd
	][, proportion := value / sum(value), keyby = .(year, sem, change.ntile)
	][, .(stock.id, year, sem, change.ntile, proportion)]

## t+1 (一期为半年）
syf <- value[stock.m, on = .(stock.id, year, sem)
	][order(stock.id, year, month)
	][, change.ntile.2 := shift(change.ntile, n = 6, fill = NA, type = "lag"), keyby = .(stock.id)
	][, proportion.2 := shift(proportion, n = 6, fill = NA, type = "lag"), keyby = .(stock.id)
	][!is.na(proportion.2)
	][, .(ret = sum(proportion.2 * month_ret)), keyby = .(year, month, change.ntile.2)]

#
Q5 <- syf[change.ntile.2 == 5] # 0.0177577  t = 2.5099, df = 155, p-value = 0.0131
t.test(Q5[, ret])

Q1 <- syf[change.ntile.2 == 1] # 0.01770859 t = 2.3612, df = 155, p-value = 0.01946
t.test(Q1[, ret])

t.test(Q5[, ret], Q1[, ret], paired = FALSE) # t = 0.015197, df = 155, p - value = 0.9879

# regression
## 各种因子以及alpha
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

Q5 <- four[Q5, on = .(year, month), nomatch = 0
	][, rit := ret - rf]
social.q5 <- lm(rit  ~ mkt_rf + smb + hml + rmw + cma + umd, Q5)# %>% summary() # (Intercept)  0.006397   0.002806   2.279  0.02406 *

Q1 <- four[Q1, on = .(year, month), nomatch = 0
	][, rit := ret - rf]
social.q1 <- lm(rit  ~ mkt_rf + smb + hml + rmw + cma + umd, Q1) # %>% summary() # (Intercept)  0.004697   0.002842   1.652  0.10054 

dif <- Q5[Q1, on = .(year, month)
	][, min := ret - i.ret] #ret为5，i.ret为1
social.qd <- lm(min ~ mkt_rf + smb + hml + rmw + cma + umd, dif)# %>% summary() # (Intercept)  0.001700   0.003437   0.495  0.62150   


stargazer(fund.q5, fund.q1, fund.qd, social.q5, social.q1, social.qd, type = "html", out = "C:/Users/shenfan/Desktop/社保/changes.doc")




## 自己算的social
#社保重仓
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)]

## 
social <- social[order(date, stock.id)
	][, .(sum.social = sum(proportion.current)), keyby = .(date, stock.id)
	][, year := year(date)
	][, quarter := quarter(date)
	][, IO.social := sum.social
	][quarter == 2 | quarter == 4
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(stock.id, year, sem, IO.social)]

IOS <- social

# 滞后
IOS <- IOS[, IO.social.2 := shift(IO.social, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, IO.social.change := IO.social - IO.social.2]

# value 来分ntile
value <- stock.s[IOS, on = .(stock.id, year, sem)
	][, changes := Dsmvosd * IO.social.change
	][!is.na(changes)
	][, change.ntile := ntile(changes, 5), keyby = .(year, sem)
	][, value := IO.social * Dsmvosd
	][, proportion := value / sum(value), keyby = .(year, sem, change.ntile)
	][, .(stock.id, year, sem, change.ntile, proportion)]

## t+1 (一期为半年）
syf <- value[stock.m, on = .(stock.id, year, sem)
	][order(stock.id, year, month)
	][, change.ntile.2 := shift(change.ntile, n = 6, fill = NA, type = "lag"), keyby = .(stock.id)
	][, proportion.2 := shift(proportion, n = 6, fill = NA, type = "lag"), keyby = .(stock.id)
	][!is.na(proportion.2)
	][, .(ret = sum(proportion.2 * month_ret)), keyby = .(year, month, change.ntile.2)]

#
Q5 <- syf[change.ntile.2 == 5] 
t.test(Q5[, ret])

Q1 <- syf[change.ntile.2 == 1] 
t.test(Q1[, ret])

t.test(Q5[, ret], Q1[, ret], paired = TRUE) 

# regression
## 各种因子以及alpha
four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

Q5 <- four[Q5, on = .(year, month), nomatch = 0
	][, rit := ret - rf]
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, Q5) %>% summary() 

Q1 <- four[Q1, on = .(year, month), nomatch = 0
	][, rit := ret - rf]
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, Q1) %>% summary() 

dif <- Q5[Q1, on = .(year, month)
	][, min := ret - i.ret] #ret为5，i.ret为1
lm(min ~ mkt_rf + smb + hml + rmw + cma + umd, dif) %>% summary()



############################################################新一轮
#中间有一步，区分这一期是不是下一期，不能简单的shift
load("IOS.RData")
IOS.m <- IOS.m[order(stock.id, year, sem)
	][, year2 := shift(year, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, sem2 := shift(sem, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, year2 := ifelse(sem2 == 2, year2 + 1, year2)
	][, sem2 := ifelse(sem2 == 2, 1, 2)
	][, yesno := ifelse(year == year2 & sem == sem2, 1, 0)
	][, holding.2 := shift(holding, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, hold.c.s := holding - holding.2
	][!yesno == 0
	][!is.na(hold.c.s)
	][, change.ntile := ntile(hold.c.s, 5), keyby = .(year, sem)
	][, proportion := hold.c.s / sum(hold.c.s), keyby = .(year, sem, change.ntile)
	][, .(stock.id, year, sem, change.ntile, proportion)]

## 变到下一期
IOS.m <- IOS.m[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]


## t+1 (一期为半年） (这里要运行上面的stock.m)
syf.s <- IOS.m[stock.m, on = .(stock.id, year, sem)
	][order(stock.id, year, month)
	][!is.na(change.ntile)
	][, .(ret = sum(proportion * month_ret)), keyby = .(year, month, change.ntile)]

#
Q5 <- syf.s[change.ntile == 5] 
t.test(Q5[, ret])

Q1 <- syf.s[change.ntile == 1] 
t.test(Q1[, ret])


t.test(Q5[, ret], Q1[, ret], paired = FALSE)


## fund
load("IOF.RData")
IOF.m <- IOF.m[order(stock.id, year, sem)
	][, year2 := shift(year, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, sem2 := shift(sem, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, year2 := ifelse(sem2 == 2, year2 + 1, year2)
	][, sem2 := ifelse(sem2 == 2, 1, 2)
	][, yesno := ifelse(year == year2 & sem == sem2, 1, 0)
	][, holding.2 := shift(holding, n = 1, fill = NA, type = "lag"), keyby = .(stock.id)
	][, hold.c.s := holding - holding.2
	][!yesno == 0
	][!is.na(hold.c.s)
	][, change.ntile := ntile(hold.c.s, 5), keyby = .(year, sem)
	][, proportion := hold.c.s / sum(hold.c.s), keyby = .(year, sem, change.ntile)
	][, .(stock.id, year, sem, change.ntile, proportion)]

## 变到下一期
IOF.m <- IOF.m[, year := ifelse(sem == 2, year + 1, year)
	][, sem := ifelse(sem == 2, 1, 2)]


## t+1 (一期为半年）
syf.f <- IOF.m[stock.m, on = .(stock.id, year, sem)
	][order(stock.id, year, month)
	][!is.na(change.ntile)
	][, .(ret = sum(proportion * month_ret)), keyby = .(year, month, change.ntile)]

#
Q5 <- syf.f[change.ntile == 5]
t.test(Q5[, ret])

Q1 <- syf.f[change.ntile == 1]
t.test(Q1[, ret])

t.test(Q5[, ret], Q1[, ret], paired = FALSE)

