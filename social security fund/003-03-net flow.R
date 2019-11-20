# social fund net flow
# all
load("socialpf.RData")
social <- social[order(fund, year, sem)
	][, .(mv = sum(MV.current, na.rm = TRUE)), keyby = .(year, sem)]

# 简单版 直接*(1+mkt return)
four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
mkt <- four[, .(year, month, mkt_rf, rf)
	][, mkt := mkt_rf + rf
	][, sem := ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6, 1, 2)
	][, .(mkt = prod(1 + mkt) - 1), keyby = .(year, sem)]

# 将mkt滞后一期
mkt <- mkt[, mkt.1 := shift(mkt, n = 1, fill = NA, type = "lead")]

# 预计的mv
all <- mkt[social, on = .(year, sem)
	][, mv.next := (1 + mkt.1) * (mv)
	][, mv.1 := shift(mv.next, n = 1, fill = NA, type = "lag")
	][, flow := (mv - mv.1) / mv.1
	][, head := "all"
	][, .(head, year, sem, flow)]

## separte
load("socialpf.RData")

social <- social[, fund.jc := str_replace_all(fund, "全国社保基金", "")
	][, fund.jc := str_replace_all(fund.jc, "组合", "")
	][, head := substring(fund.jc, 1, 1)]

social <- social[order(fund, year, sem)
	][, .(mv = sum(MV.current, na.rm = TRUE)), keyby = .(head, year, sem)]

# 预计的mv
separte <- mkt[social, on = .(year, sem)
	][, mv.next := (1 + mkt.1) * (mv)
	][, mv.1 := shift(mv.next, n = 1, fill = NA, type = "lag")
	][, flow := (mv - mv.1) / mv.1
	][, .(head, year, sem, flow)]

figure <- rbindlist(list(all, separte))

# date
date.q <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间q.xlsx")
date.q <- as.data.table(date.q)
date.q <- date.q[, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 1 | quarter == 2, 1, 2)
	][, .SD[.N], keyby = .(year, sem)]

figure <- date.q[figure, on = .(year, sem)]

xbreaks <- c('2004-06-30', '2006-06-30', '2008-06-30', '2010-06-30', '2012-06-30', '2014-06-30', '2016-06-30')

ggplot(figure[head=="all"], aes(x = date, y = flow)) +
	geom_bar(stat = "identity") +
	#	geom_histogram(colour = "black") +
	scale_y_continuous(expand = c(0, 0)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
	legend.text = element_text(size = 8),
	legend.title = element_blank(),
	legend.position = c(0.15, 0.85),
)


