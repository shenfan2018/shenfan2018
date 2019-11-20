#加入csmar的inflow outflow
sharechange <- read_excel("C://Users//shenfan//Desktop//data//2//份额变动文件//Fund_ShareChange.xlsx")
sharechange <- as.data.table(sharechange)
setnames(sharechange, 2:11, c("class", "id", "type", "start", "DateQ", "beginning", "purchase", "redemption", "split", "end"))
#调格式,若用5和6的话，beginning有问题
sharechange <- sharechange[id != "基金代码'", .SD
	][id != "没有单位'"
	][, type := as.numeric(as.character(type))
	][type == 1 | type == 2 | type == 3 | type == 4, .SD
	][, DateQ := as.Date(as.character(DateQ))
	][, beginning := as.numeric(as.character(beginning))
	][, purchase := as.numeric(as.character(purchase))
	][, redemption := as.numeric(as.character(redemption))
	][, end := as.numeric(as.character(end))
	][type == 1 | type == 2, sem := 1
	][type == 3 | type == 4, sem := 2
	][, year := year(DateQ)]

#计算半年的
sharechange <- sharechange[order(id, DateQ)
	][, purchase2 := sum(purchase, na.rm = FALSE), keyby = .(id, year, sem)
	][, redemption2 := sum(redemption, na.rm = FALSE), keyby = .(id, year, sem)
	][type == 1 | type == 3, inflow := purchase2 / beginning
	][type == 1 | type == 3, outflow := redemption2 / beginning
	][type == 1 | type == 3, netflow := (purchase2 - redemption2) / beginning
	][type == 1 | type == 3, .SD]

sharechange <- sharechange[, c("id", "year", "sem", "purchase2", "redemption2", "beginning", "end", "inflow", "outflow", "netflow")]

#fund style(wind+csmar)
fund.csmar <- read_excel("C://Users//shenfan//Desktop//data//基金类型风格//Fund_MainInfo.xlsx")
fund.csmar <- as.data.table(fund.csmar)
setnames(fund.csmar, 1:2, c("id", "name"))
fund.csmar <- fund.csmar[name != "基金全称'", .SD
	][name != "没有单位'", .SD
	][, name := NULL]

fund.wind <- read_excel("C://Users//shenfan//Desktop//data//基金类型风格//windstyle.xlsx")
fund.wind <- as.data.table(fund.wind)
setnames(fund.wind, 1:6, c("code", "name", "category2", "category1", "style1", "style2"))
fund.wind <- fund.wind[, id := substring(code, 1, 6)
	][, c("style1", "style2", "name", "category1") := NULL]
fund.category <- fund.csmar[fund.wind, on = .(id)]
#match
sharechange <- fund.category[sharechange, on = .(id), nomatch = 0]

#选择混合型和股票型
sharechange <- sharechange[Category == "股票型基金" | Category == "混合型基金", .SD
	][category2 == "偏股混合型基金" | category2 == "普通股票型基金", .SD]

###################################################################
# mutual fund all flow
flow <- sharechange

flow <- flow[, end := shift(beginning, n = 1, fill = NA, type = "lead"), keyby = .(id)
	][, end2 := beginning + purchase2 - redemption2
	][, .(beginning = sum(beginning, na.rm = TRUE), purchase = sum(purchase2, na.rm = TRUE), redemption = sum(redemption2, na.rm = TRUE), end = sum(end, na.rm = TRUE)), keyby = .(year, sem)
	][, net := purchase - redemption
	][, flow.rate := (end - beginning) / beginning]

##
date <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][quarter == 2 | quarter == 4]

flow <- date[flow, on = .(year, sem), nomatch = 0]

ggplot(flow, aes(x = date)) +
	geom_bar(aes(y = flow.rate), stat = "identity") 


## 基金的MV
load("IOF.RData")
mktfund <- IOF.m[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(fundvalue = sum(holding)), keyby = .(year, sem)
	][, fundvalue2 := shift(fundvalue, n = 1, fill = NA, type = "lag")
	][, mkt.rate := (fundvalue - fundvalue2) / fundvalue2]

date <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][quarter == 2 | quarter == 4]

mktfund <- date[mktfund, on = .(year, sem), nomatch = 0]

# 加入大盘上证综指
szzz <- read_excel("C:/Users/shenfan/Desktop/社保/data/上证综指.xlsx")
szzz <- as.data.table(szzz)
szzz <- szzz[, quarter := quarter(date)
	][, year := year(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .(index = mean(index)), keyby = .(year, sem)]

data <- mktfund[flow, on = .(year, sem)]
	
data <- szzz[data, on = .(year, sem)
	][, .(date, flow.rate, mkt.rate, index)]

a <- data[, .(date, flow.rate, index)]
a <- melt(a, id.vars = "date")

xbreaks <- c('2004-06-30', '2005-06-30', '2006-06-30', '2007-06-30', '2008-06-30', '2009-06-30', '2010-06-30', '2011-06-30', '2012-06-30', '2013-06-30', '2014-06-30', '2015-06-30', '2016-06-30', '2017-06-30')

ggplot(a) +
	facet_grid(variable ~ ., scales = "free_y") +
	geom_line(data = a[variable == "index"],
	aes(x = date, y = value), color = "blue") +
	geom_bar(data = a[variable == 'flow.rate'],
	aes(x = date, y = value), stat = 'identity', color = "steelblue") +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks)

theme(axis.text.x = element_text(size = 1),
   axis.text = element_text(size = 5),
   axis.title = element_blank(),
   axis.line = element_line(),
#   strip.text = element_text(family = "RMN", size = 5),
   strip.placement = "outside"
   )

#########用data
data<- melt(data, id.vars = "date")

ggplot(data) +
	geom_line(data = data[variable == "flow.rate"],
	aes(x = date, y = value), color = "blue") +
	geom_line(data = data[variable == "mkt.rate"],
	aes(x = date, y = value), color = "red")

## 
cor.test(data[, mkt.rate], data[, flow.rate], alternative = "two.sided", method = "pearson", conf.level = 0.95)






