fund.TK <- fread("C:/Users/shenfan/Desktop/prospect value/data/半年/data31.csv")
fund.TK <- fund.TK[, id := as.character(id)
	][, id := str_pad(id, 6, side = "left", pad = "0")]

cash1 <- read_excel('C:/Users/shenfan/Desktop/prospect value/data/股票型 银行存款占基金净值之比.xlsx')
cash2 <- read_excel('C:/Users/shenfan/Desktop/prospect value/data/混合型 银行存款占基金净值之比.xlsx')
cash <- rbindlist(list(cash1, cash2))
cash <- as.data.table(cash)
setnames(cash,2,"name")
cash <- cash[!is.na(name)]
date <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2014-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-29", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30", "2018-09-30", "2018-12-31")
setnames(cash, 1:62, c("code", "name", date))
cash = melt(cash, id.vars = 1:2, measure.vars = 3:62)
setnames(cash, 3:4, c("date", "cash"))
cash <- cash[, id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)]
cash <- cash[, .(id, year, quarter, cash)]

fund.TK <- cash[fund.TK, on = .(id, year, quarter)]


write.csv(fund.TK, "C://Users//shenfan//Desktop//addcash.csv")

