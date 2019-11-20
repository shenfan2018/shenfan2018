# fund return
# 加入monthly return
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/fund-NAV.RData")
# 月度的收益率
ret <- data.NAV[, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][, AdjustedNAVGrowth2 := 1 + AdjustedNAVGrowth
	][, .(month_return = prod(AdjustedNAVGrowth2, na.rm = TRUE) - 1), keyby = .(id, year, quarter, month)]

# 加入时间
date <- read_excel("C:/Users/shenfan/Desktop/prospect value/date.xlsx")
date <- as.data.table(date)
date <- date[, year := year(date)
	][, month := month(date)]

ret <- date[ret, on = .(year, month), nomatch = 0]

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
ret <- fund.category[ret, on = .(id), nomatch = 0]

#选择混合型和股票型
ret <- ret[Category == "股票型基金" | Category == "混合型基金", .SD
	][category2 == "偏股混合型基金" | category2 == "普通股票型基金", .SD]

ret <- ret[, .(id, category2, date, year, month, month_return)]

write.csv(ret, "C://Users//shenfan//Desktop//fund-return.csv")



############################################################################
load("C:/Users/shenfan/source/repos/shenfan2018/fund 2004-2017/stock.RData")
#整理
stock.w <- stock
stock.w <- stock.w[date != "没有单位'", .SD
	][date != "交易日期'", .SD
	][, Clsprc := as.numeric(as.character(Clsprc))
	][, Dretwd := as.numeric(as.character(Dretwd))
	][, date := as.Date(date)]

# 各种因子以及alpha
four <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four <- as.data.table(four)
four <- four[, date := as.Date(trddy)
	][, mkt := mkt_rf + rf
	][, .(date, mkt)]

a <- four[stock.w, on = .(date)]

a <- a[order(stock.id, date)
	][, year := year(date)
	][, week := isoweek(date)
	][, month := month(date)
	][, Dretwd2 := Dretwd + 1
	][, mkt2 := mkt + 1
	][, year := ifelse(month == 12 & week == 1, year + 1, year)
	][, week_return := prod(Dretwd2) - 1, keyby = .(stock.id, year, week)
	][, week_mkt := prod(mkt2) - 1, keyby = .(stock.id, year, week)
	][, .SD[.N], keyby = .(stock.id, year,  week)]

a <- a[, ret_mkt := week_return - week_mkt
	][, .(stock.id, date, year, month, week, week_return, week_mkt, ret_mkt)]


write.csv(a, "C://Users//shenfan//Desktop//week return_mkt.csv")


###
week1 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week stock/TRD_Week.xlsx")
week2 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week stock/TRD_Week2.xlsx")
week3 <- read_excel("C:/Users/shenfan/Desktop/prospect value/week stock/TRD_Week3.xlsx")
week <- rbindlist(list(week1, week2, week3))
rm(week1, week2, week3)

stock.w <- week
stock.w <- as.data.table(stock.w)
stock.w<- stock.w[Stkcd != "没有单位'", .SD
	][Stkcd != "证券代码'", .SD
	][, Wretwd := as.numeric(as.character(Wretwd))
	][, date := as.Date(Clsdt)
	][, week := substring(Trdwnt, 6, 7)
	][, week := as.numeric(week)
	][, month := month(date)
	][order(Stkcd, date)
	][, .(Stkcd, Trdwnt, date, week, Ndaytrd, Wretwd)]

# factor
four <- fread("C:/Users/shenfan/Desktop/prospect value/week stock/2019-04-01-three_four_five_factor_weekly/three_four_five_factor_weekly/fivefactor_weekly.csv")
four <- as.data.table(four)
four <- four[, date := as.Date(trdwk)
	][, mkt := mkt_rf + rf
	][, .(date, mkt)
	][, week := isoweek(date)]


stock.w <- four[stock.w, on = .(date)
	][, ret_mkt := Wretwd - mkt
	][, .(Stkcd, Trdwnt, date, Ndaytrd, Wretwd, mkt, ret_mkt)]

write.csv(stock.w, "C://Users//shenfan//Desktop//week return_mkt.csv")




