# social security fund 
load("social_ret.RData")

social.ret <- social.ret[, fund.jc := str_replace_all(fund, "全国社保基金", "")
	][, fund.jc := str_replace_all(fund.jc, "组合", "")
	][, head := substring(fund.jc, 1, 1)]

# 按照类别
social.ret <- social.ret[, .(ret.s = mean(ret.s)), keyby = .(head, year, month)]

# date
date.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间.xlsx")
date.m <- as.data.table(date.m)
date.m <- date.m[, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)]

social.ret <- date.m[social.ret, on = .(year, month)]

xbreaks <- c('2004-06-30', '2006-06-30', '2008-06-30', '2010-06-30', '2012-06-30', '2014-06-30', '2016-06-30')

# 累计
separte <- social.ret[, prod := cumprod(1 + ret.s), keyby = .(head)]

### all
load("social_ret.RData")
social.ret <- social.ret[, .(ret.s = mean(ret.s)), keyby = .(year, month)]

social.ret <- date.m[social.ret, on = .(year, month)]

all <- social.ret[, prod := cumprod(1 + ret.s)
	][, head := "all"
	][, .(date, year, month, head, ret.s, prod)]

figure <- rbindlist(list(all, separte))


ggplot(figure, aes(x = date)) +
	geom_line(aes(y = ret.s, linetype = head)) +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 8),
#	legend.title = element_blank(),
	legend.position = c(0.05, 0.85),
)



########################
load("C:/Users/shenfan/source/repos/Wechat/低开/index.RData")
sz <- data[code == "000001"
	][, date := as.Date(date)
	][date < as.Date("2019-01-01")
	][order(date)]

xbreaks <- c('2004-12-31', '2006-12-31', '2008-12-31', '2010-12-31', '2012-12-31', '2014-12-31', '2016-12-31', '2018-12-31')

ggplot(sz, aes(x = date)) +
	geom_line(aes(y = close)) +
	geom_vline(aes(xintercept = as.Date("2010-12-31")), colour = 'black') +
	#geom_vline(aes(xintercept = as.Date("2007-12-31")), colour = 'black', linetype = "dashed") +
	#geom_vline(aes(xintercept = as.Date("2013-12-31")), colour = 'black', linetype = "dashed") +
	#geom_hline(aes(yintercept = 0), colour = 'black') +
	theme_bw() +
	scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	theme(
	panel.border = element_blank(),
	panel.background = element_blank(),
	axis.line = element_line(linetype = 1),
#	plot.title = element_text(hjust = 0.5),
#	legend.text = element_text(size = 8),
#	legend.title = element_blank(),
	legend.position = c(0.05, 0.85),
)



######################################################################### k线图
library(quantmod)

data <- data.frame(Date=sz[,4], Open = sz[, 5], High = sz[, 6], Low = sz[, 7], Close = sz[, 8], Volume = sz[, 9])

rownames(data) <- as.Date(as.character(sz$date), "%Y%m%d")

chartSeries(GOOG)



################################## 
# other way
FT_Kplot<-function(OPEN,HIGH,LOW,CLOSE,DATE)
	{N <- length(OPEN)
	w <- 0.3
	D <- CLOSE - OPEN
	par(family = 'serif')
	# 作图
	plot(c(1:N), CLOSE, type = 'n', xaxt = 'n',
   xlab = '时间', ylab = '价格', font.axis = 1.5)
	title(main = 'K线图', cex = 2, col = 'black')
	for (i in 1:N)
		{ lines(c(i, i), c(LOW[i], HIGH[i]), col = 'black', lwd = 1)
		x <- c(i - w, i - w, i + w, i + w)
		y <- c(OPEN[i], CLOSE[i], CLOSE[i], OPEN[i])
		if (D[i] < 0)
			{
			polygon(x, y, col = 'green', border = 'green')
} else
		{
			polygon(x, y, col = 'red', border = 'red')
		}
	}
	Index<-seq(from=1,to=N,length=5)
	Index <- round(Index)
	Text <- DATE[Index]
	axis(side = 1, Index, labels = Text, cex.axis = 1)
}

date <- sz[, 4]
open <- sz[, 5]
high <- sz[, 6]
low <- sz[, 7]
close <- sz[, 8]
DATE <- substr(date, 1, 10)

FT_Kplot(open[c(1:100)], high[c(1:100)], low[c(1:100)], close[c(1:100)], DATE[c(1:100)])


#############################################################
# 人大经济
library(tidyverse)
library(gridExtra)
library(quantmod) # 要用到SMA函数

# 为了解决日期作为横坐标会出现休市日的情况，需要以下的变量辅助
row_len <- nrow(sz)
breaks <- seq(1, row_len, 10)
labels <- sz$date[breaks]

# 作K线图
p1 <- sz %>%
    arrange(date) %>%
    mutate(ma5 = SMA(close, n = 5, align = "right"),
		 ma10 = SMA(close, n = 10, align = "right"),
		 date_axis = row_number()) %>%
		 ggplot(aes(x = date_axis)) +
	     geom_boxplot(aes(lower = pmin(close, open),
				   middle = close,
				   upper = pmax(close, open),
				   ymin = low,
				   ymax = high,
				   group = date,
				   fill = open > close),
			   stat = "identity",
			   show.legend = FALSE) +
			   geom_line(aes(y = ma5), color = "blue3") +
			   geom_line(aes(y = ma10), color = "red") +
			   scale_x_continuous(breaks = breaks,
					 labels = NULL,
					 expand = c(0, 0)) +
				     theme(axis.ticks.x = element_blank(),
		axis.title = element_blank(),
		axis.text.y = element_text(margin = margin(l = 8)))

# 作成交量图
p2 <- sz %>%
    arrange(date) %>%
    mutate(vol_ma5 = SMA(volume, n = 5, align = "right"),
		 vol_ma10 = SMA(volume, n = 10, align = "right"),
		 date_axis = row_number()) %>%
		 ggplot(aes(x = date_axis, y = volume)) +
	     geom_bar(stat = "identity",
		   aes(fill = open > close),
		   show.legend = FALSE) +
		   geom_line(aes(y = vol_ma5), color = "blue3") +
		   geom_line(aes(y = vol_ma10), color = "red") +
		   scale_x_continuous(breaks = breaks,
					 labels = format(labels, "%m-%d"),
					 expand = c(0, 0)) +
				     scale_y_continuous(expand = c(0, 0.5)) +
				     theme(axis.title = element_blank())

# 组合
grid.arrange(p1, p2, nrow = 2, heights = 2:1)


#################################################################### me
load("C:/Users/shenfan/source/repos/Wechat/低开/index.RData")
sz <- data[code == "000001"
	][, date := as.Date(date)
	][date < as.Date("2019-01-01")
	][order(date)
	][date > as.Date("2017-12-31")]

xbreaks <- c('2004-12-31', '2006-12-31', '2008-12-31', '2010-12-31', '2012-12-31', '2014-12-31', '2016-12-31', '2018-12-31')

xbreaks <- c('2004-12-31', '2006-12-31', '2008-12-31', '2010-12-31', '2012-12-31', '2014-12-31', '2018-12-31', '2018-12-31')

ggplot(sz, aes(x = date)) +
	geom_boxplot(aes(lower = pmin(close, open),
	 middle = close,
	 upper = pmax(close, open),
	 ymin = low,
	 ymax = high,
	 group = date,
	 fill = open > close),
	 stat = "identity",
	 show.legend = FALSE) +
	 scale_x_time(breaks = as.Date(xbreaks), labels = xbreaks) +
	 theme(axis.ticks.x = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_text(margin = margin(l = 8)))