load('uel_shock.RData')

############################################################### NA 变成0, 分组
uel.gz <- uel.gz[, social.g := ntile(IO.social.0, 5), keyby = .(quarter) 
	][, social.buy := ifelse(IO.social.0 == 0, 0, 1)]

## mean
a <- uel.gz[, .(uel = mean(uel)), keyby = .(quarter, social.g)]

#  ##difference
#a <- uel.gz[, .(dif = t.test(uel.gz[quarter == 2, uel], uel.gz[quarter == 3, uel])$estimate, t = t.test(uel.gz[quarter == 2, uel], uel.gz[quarter == 3, uel])$statistic), keyby = .(social.g)]

# sort
t.test(uel.gz[quarter == 3 & social.g == 5, uel], uel.gz[quarter == 3 & social.g == 1, uel])
t.test(uel.gz[quarter == 2, uel], uel.gz[quarter == 3, uel])

############################################################################ NA 不变
uel.gz <- uel.gz[, social.g := ntile(IO.social, 5), keyby = .(quarter)
	][, social.buy := ifelse(IO.social == 0, 0, 1)
	][!is.na(social.g)]

## mean
a <- uel.gz[, .(uel = mean(uel)), keyby = .(quarter, social.g)]

# sort
t.test(uel.gz[quarter == 3 & social.g == 5, uel], uel.gz[quarter == 3 & social.g == 1, uel])
t.test(uel.gz[quarter == 2, uel], uel.gz[quarter == 3, uel])

###################################################################### buy and not buy
t.test(uel.gz[quarter == 3 & social.buy == 1, uel], uel.gz[quarter == 3 & social.buy == 0, uel])

# 2015.2 buy  2015.3 buy not buy
uel.buy <- uel.gz[order(stock.id, year, quarter)
	][, social.buy.2 := shift(social.buy, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)
	][, uel2 := shift(uel, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)
	][quarter == 2 & social.buy == 1]
# 2015.2 2015.3均持有social.buy.2为1，2014.2持有，2015.3没有持有，social.buy.2为0
# 2015.2
t.test(uel.buy[, uel])
t.test(uel.buy[social.buy.2 == 1, uel2], uel.buy[social.buy.2 == 0, uel2])
t.test(uel.buy[, uel], uel.buy[social.buy.2 == 0, uel2])

# 2015.2 not buy  2015.3 buy not buy
uel.nbuy <- uel.gz[order(stock.id, year, quarter)
	][, social.buy.2 := shift(social.buy, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)
	][, uel2 := shift(uel, n = 1, fill = NA, type = "lead"), keyby = .(stock.id)
	][quarter == 2 & social.buy == 0]
# 2015.2没有持有，2015.3买了，social.buy.2为1，2014.2没有，2015.3依然没有持有，social.buy.2为0
# 2015.2
t.test(uel.nbuy[, uel])
# 2015.3
t.test(uel.nbuy[social.buy.2 == 1, uel2], uel.nbuy[social.buy.2 == 0, uel2])
t.test(uel.nbuy[, uel], uel.nbuy[social.buy.2 == 0, uel2])


## add mutual fund
############################################################### NA 变成0, 分组
uel.gz <- uel.gz[, fund.g := ntile(IO.fund.0, 5), keyby = .(quarter)
	][, fund.buy := ifelse(IO.fund.0 == 0, 0, 1)]

## mean
a <- uel.gz[, .(uel = mean(uel)), keyby = .(quarter, fund.g)]

#  ##difference
#a <- uel.gz[, .(dif = t.test(uel.gz[quarter == 2, uel], uel.gz[quarter == 3, uel])$estimate, t = t.test(uel.gz[quarter == 2, uel], uel.gz[quarter == 3, uel])$statistic), keyby = .(social.g)]

# sort
t.test(uel.gz[quarter == 3 & fund.g == 5, uel], uel.gz[quarter == 3 & fund.g == 1, uel])
t.test(uel.gz[quarter == 2, uel], uel.gz[quarter == 3, uel])

###################################################################### buy and not buy
t.test(uel.gz[quarter == 3 & fund.buy == 0, uel], uel.gz[quarter == 3 & fund.buy == 1, uel])


# add national 

############################################################### NA 变成0, 分组
uel.gz <- uel.gz[, national.g := ntile(IO.national.0, 5), keyby = .(quarter)
	][, national.buy := ifelse(IO.national.0 == 0, 0, 1)]

## mean
a <- uel.gz[, .(uel = mean(uel)), keyby = .(quarter, national.g)]

#  ##difference
#a <- uel.gz[, .(dif = t.test(uel.gz[quarter == 2, uel], uel.gz[quarter == 3, uel])$estimate, t = t.test(uel.gz[quarter == 2, uel], uel.gz[quarter == 3, uel])$statistic), keyby = .(social.g)]

# sort
t.test(uel.gz[quarter == 3 & national.g == 5, uel], uel.gz[quarter == 3 & national.g == 1, uel])



