# marketvalue
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)]

# 选择1,5,6
social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "fund.company"))
social.m <- social.m[, fund.company := str_replace_all(fund.company, "管理有限公司", "")
	][!is.na(fund.company), .SD
	][, fund.jc := str_replace_all(fund, "全国社保基金", "")
	][, fund.jc := str_replace_all(fund.jc, "组合", "")
	][, head := substring(fund.jc, 1, 1)
	][head == 1 | head == 5 | head == 6
	][, .(fund, fund.company)]

social <- social.m[social, on = .(fund), nomatch = 0]

################################################################### 1

mktvalue <- social[, .(t.MV = sum(MV.current, na.rm = TRUE)), keyby = .(date)
	][, t.MV1 := shift(t.MV, n = 1, fill = NA, type = "lag")
	][, rate := (t.MV - t.MV1) / t.MV1
	][, year := year(date)
	][, quarter := quarter(date)]

ggplot(mktvalue, aes(x = date)) +
	geom_bar(aes(y = rate), stat = "identity")

# 加入vol
mkt <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
mkt <- as.data.table(mkt)
mkt <- mkt[, date := as.Date(trddy)
	][, mkt := mkt_rf + rf
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(vol = sd(mkt)), keyby = .(year, quarter)]

mktvalue <- mkt[mktvalue, on = .(year, quarter)
	][, vol2 := vol * 10]

# 加入大盘上证综指
szzz <- read_excel("C:/Users/shenfan/Desktop/社保/data/上证综指.xlsx")
szzz <- as.data.table(szzz)
szzz <- szzz[, quarter := quarter(date)
	][, year := year(date)
	][, .(index = mean(index)), keyby = .(year, quarter)]

mktvalue <- szzz[mktvalue, on = .(year, quarter)
	][, index2 := index / 4000]


ggplot(mktvalue, aes(x = date)) +
	geom_bar(aes(y = rate), stat = "identity") +
	geom_line(aes(y = vol2), colour = "blue") +
	geom_line(aes(y = index2), colour = "red") 

a <- mktvalue[, .(date, rate, index)]

a <- melt(a, id.vars = "date")
a <- a[order(-variable, date)]
xbreaks <- c('2004-06-30', '2005-06-30', '2006-06-30', '2007-06-30', '2008-06-30', '2009-06-30', '2010-06-30', '2011-06-30', '2012-06-30', '2013-06-30', '2014-06-30', '2015-06-30', '2016-06-30', '2017-06-30')

ggplot(a) +
	facet_grid(variable ~ ., scales = "free_y") +
				  	geom_line(data = a[variable == "index" ],
					  aes(x = date, y = value), color = "blue") +
				  	geom_bar(data = a[variable == 'rate'],
					aes(x = date, y = value), stat = 'identity', color = "steelblue") +
					theme_bw() +
					scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks)

theme(axis.text.x = element_text(size = 1),
				 axis.text = element_text(size = 5),
	 			   axis.title = element_blank(),
	 			   axis.line = element_line(),
#	 			   strip.text = element_text(family = "RMN", size = 5),
	 			   strip.placement = "outside"
				 )


a <- mktvalue[, .(date, rate, vol)]

a <- melt(a, id.vars = "date")

ggplot(a) +
	facet_grid(variable ~ ., scales = "free_y") +
		geom_line(data = a[variable == 'vol'],
					  aes(x = date, y = value), color = "red") +
  					geom_bar(data = a[variable == 'rate'],
					aes(x = date, y = value), stat = 'identity', color = "steelblue") +
					theme_bw() +
					scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks)
					theme(
				 axis.text = element_text(size = 5),
				 axis.title = element_blank(),
				 axis.line = element_line(),
#	 			   strip.text = element_text(family = "RMN", size = 5),
				 strip.placement = "outside"
				 )


########################################################### 2
# 加入加仓还是减仓的因素，mkt的return
mktvalue <- social[, .(t.MV = sum(MV.current, na.rm = TRUE)), keyby = .(date)
	][, t.MV1 := shift(t.MV, n = 1, fill = NA, type = "lag")
	][, rate := (t.MV - t.MV1) / t.MV1
	][, year := year(date)
	][, quarter := quarter(date)]

szzz <- read_excel("C:/Users/shenfan/Desktop/社保/data/上证综指.xlsx")
szzz <- as.data.table(szzz)
szzz <- szzz[, quarter := quarter(date)
	][, year := year(date)
	][, .(index = mean(index)), keyby = .(year, quarter)
	][, index2 := shift(index, n = 1, fill = NA, type = "lag")
	][, mkt.return := (index - index2) / index2]

a <- szzz[mktvalue, on = .(year, quarter)
	][, rate222 := rate - mkt.return]

#a <- a[, .(date, rate222, index)]

#a <- melt(a, id.vars = "date")
#a <- a[order(-variable, date)]
#xbreaks <- c('2004-06-30', '2005-06-30', '2006-06-30', '2007-06-30', '2008-06-30', '2009-06-30', '2010-06-30', '2011-06-30', '2012-06-30', '2013-06-30', '2014-06-30', '2015-06-30', '2016-06-30', '2017-06-30')

#ggplot(a) +
	#facet_grid(variable ~ ., scales = "free_y") +
					#geom_line(data = a[variable == "index"],
					  #aes(x = date, y = value), color = "blue") +
  					#geom_bar(data = a[variable == 'rate222'],
					#aes(x = date, y = value), stat = 'identity', color = "steelblue") +
					#theme_bw() +
					#scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks)

#theme(axis.text.x = element_text(size = 1),
				 #axis.text = element_text(size = 5),
				 #axis.title = element_blank(),
				 #axis.line = element_line(),
				 #strip.placement = "outside"
				 #)

#lm(rate222 ~ index, data = a)%>% summary()

#############################################################