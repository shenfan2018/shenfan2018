load("socialpf.RData")
social <- social[order(fund, year, sem)
	][, .(fund, fund.company, date, stock.id, MV.current)
	][, .(mv = sum(MV.current, na.rm = TRUE)), keyby = .(date)]


xbreaks <- c('2004-06-30', '2006-06-30', '2008-06-30', '2010-06-30', '2012-06-30', '2014-06-30', '2016-06-30')

ggplot(social, aes(x = date, y = mv)) +
	geom_bar(stat = "identity") +
	#	geom_histogram(colour = "black") +
	scale_y_continuous(expand = c(0, 0)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	labs(title = 'assets under management') +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
	legend.text = element_text(size = 8),
	legend.title = element_blank(),
	legend.position = c(0.15, 0.85),
)


# mutual fund
# 删除非该日期有的
load("portfolio.RData")
portfolio <- portfolio[order(id, date)
	][, .(mv = sum(MarketValue, na.rm = TRUE)), keyby = .(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .SD[.N], keyby = .(year, sem)]


ggplot(portfolio, aes(x = date, y = mv)) +
	geom_bar(stat = "identity") +
	#	geom_histogram(colour = "black") +
	scale_y_continuous(expand = c(0, 0)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	labs(title = 'assets under management') +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
	legend.text = element_text(size = 8),
	legend.title = element_blank(),
	legend.position = c(0.15, 0.85),
)