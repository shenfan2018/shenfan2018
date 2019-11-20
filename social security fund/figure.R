#社保重仓
EPU <- read_excel("C:/Users/shenfan/Desktop/社保/data/China_Policy_Uncertainty_Data.xlsx")
EPU <- as.data.table(EPU)
setnames(EPU, 3, "EPU")
EPU <- EPU[-(.N)
	][, year := as.numeric(year)]

# 各种因子以及alpha
four <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four <- as.data.table(four)
four <- four[, date := as.Date(trddy)]
mkt <- four[, mkt := mkt_rf + rf
	][, .(date, mkt)
	][, year := year(date)
	][, month := month(date)
	][, Date := substring(date, 1, 7)
	][, .(vol = sd(mkt)), keyby = .(year, month, Date)]

SJ <- mkt[EPU, on = .(year, month)
	][year > 2003
	][year < 2018]


SJ <- SJ[, Date := as.Date(date)]

date <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)]

SJ <- date[SJ, on = .(year, month)]

SJ <- SJ[, p90_vol := quantile(vol, 0.9)
	][, d_epu65 := ifelse(EPU > quantile(EPU, 0.65), 1, 0)]



ggplot(SJ, aes(x = date)) +
	geom_line(aes(y = vol)) +
	geom_line(aes(y = p90_vol)) +
	geom_ribbon(data = subset(SJ), aes(xmin = "2008-03-01", xmax = "2008-03-01"), aes(ymin = 0, ymax = 0.05240369))








	+

	scale_x_discrete(c("2004-06", "2007-06", "2010-06", "2013-06", "2017-06")),
	 + labels = date_format("%Y-%m")
)
