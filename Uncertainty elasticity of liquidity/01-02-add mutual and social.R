# annual uel
uel <- fread("C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/uel.csv")
uel <- uel[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, year := year - 1]

time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")

# add social
IOS <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/社保基金持股比例.xlsx')
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IO.social", "date"))

IOS <- IOS[!is.na(IO.social)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, year := year(date)]

# avg
IOS.avg <- IOS[, .(social.avg = mean(IO.social)), keyby = .(stock.id, year)]
# match
uel <- IOS.avg[uel, on = .(stock.id, year)]

# end
IOS.end <- IOS[, quarter := quarter(date)
	][quarter == 4
	][, .(social.end = IO.social), keyby = .(stock.id, year)]
# match
uel <- IOS.end[uel, on = .(stock.id, year)]

# add mutual fund
IOF <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/基金持股比例.xlsx')
IOF <- as.data.table(IOF)
setnames(IOF, 1:58, c("code", "name", time))
IOF = melt(IOF, id.vars = c("code", "name"), measure.vars = time)
setnames(IOF, c("value", "variable"), c("IO.fund", "date"))

IOF <- IOF[!is.na(IO.fund)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, year := year(date)]

# avg
IOF.avg <- IOF[, .(fund.avg = mean(IO.fund)), keyby = .(stock.id, year)]
# match
uel <- IOF.avg[uel, on = .(stock.id, year)]

# end
IOF.end <- IOF[, quarter := quarter(date)
	][quarter == 4
	][, .(fund.end = IO.fund), keyby = .(stock.id, year)]
# match
uel <- IOF.end[uel, on = .(stock.id, year)]

# save
save(uel, file = "uel.RData")


##################################################################################################
# annual uel



# match
uel <- IO[uel, on = .(stock.id, year)]

save(uel, file = 'uel2.RData')
