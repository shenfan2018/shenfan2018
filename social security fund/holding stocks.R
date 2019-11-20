#社保重仓
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)]

social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "fund.company"))
social.m <- social.m[, fund.company := str_replace_all(fund.company, "管理有限公司", "")
	][!is.na(fund.company), .SD]

social.pf <- social.m[social, on = .(fund)
	][quarter == 2 | quarter == 4
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(fund, fund.company, date, year, sem, stock.id, MV.current)]


#基金portfolio
#csamr上的
#读取个股持仓(这里整体选择是年报和中报，且数据从2004.1.1-2018.1.1
pf1 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock1.xlsx")
pf2 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock2.xlsx")
pf3 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock3.xlsx")
pf4 <- read_excel("C://Users//shenfan//Desktop//data//个股持仓//2//Fund_Portfolio_Stock4.xlsx")
pf <- rbindlist(list(pf1, pf2, pf3, pf4))
rm(pf1, pf2, pf3, pf4)
pf <- as.data.table(pf) #pf为个股持仓原始文件，误删
#将数据变成portfolio
portfolio <- pf
portfolio <- as.data.table(portfolio)
#选择中报和年报
portfolio <- portfolio[ReportTypeID == 5 | ReportTypeID == 6, .SD]
#整理，包括去除不要的列和enddate改名
setnames(portfolio, "EndDate", "date")
setnames(portfolio, "Symbol", "stock.id")
setnames(portfolio, 2, "id")
portfolio <- portfolio[, c("FundID", "ReportTypeID", "Startdate", "InvestmentType") := NULL]
#让proportion等变成数字,这个proportion是占净值比例
portfolio <- portfolio[, Proportion := as.numeric(as.character(Proportion))
	][, Rank := as.numeric(as.character(Rank))
	][, MarketValue := as.numeric(as.character(MarketValue))
	][, Shares := as.numeric(as.character(Shares))
	][, date := as.Date(as.character(date))]

# 整理
fund.pf <- portfolio[order(id, date, Rank)
	][, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(id, date, year, sem, stock.id, Proportion)]


fund.co <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人.xlsx")
fund.co <- as.data.table(fund.co)
setnames(fund.co, 1:3, c("code", "name", "fund.company"))
fund.co <- fund.co[, id := substring(code, 1, 6)
	][, c("id", "fund.company")]

fund.pf <- fund.co[fund.pf, on = .(id)]

# 均持有的
same <- fund.pf[social.pf, on = .(fund.company, stock.id, year, sem)
	][!is.na(id)
	][, .(buytime = (.N)), keyby = .(fund.company, stock.id, year, sem)
	][!is.na(fund.company)
	][, .(stock.id = unique(stock.id), type = "same"), keyby = .(year, sem)]

# 基金买的但社保基金没买的
fund.only <- social.pf[fund.pf, on = .(fund.company, stock.id, year, sem)
	][is.na(fund)
	][, .(stock.id = unique(stock.id), type = "fund.only"), keyby = .(year, sem)]

# 社保买的但基金没买
social.only <- fund.pf[social.pf, on = .(fund.company, stock.id, year, sem)
	][is.na(id)
	][, .(stock.id = unique(stock.id), type = "social.only"), keyby = .(year, sem)]

# proportion
load("stock.RData")
stock.s <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, .SD[.N], keyby = .(stock.id, year, sem)
	][, .(stock.id, year, sem, Dsmvosd)]


hold.stock <- same[stock.s, on = .(stock.id, year, sem)]
hold.stock <- social.only[hold.stock, on = .(stock.id, year, sem)]
hold.stock <- fund.only[hold.stock, on = .(stock.id, year, sem)]

# nothing是一方面大家都没买，还有是都买了 ，但是不是同一个公司
hold.stock <- hold.stock[, types := i.type.1
	][is.na(types), types := i.type
	][is.na(types), types := type
	][is.na(types), types := "nothing"
	][i.type == "social.only" & type == "fund.only", types := "all"
	][, .(stock.id, year, sem, types, Dsmvosd)]

hold.stock <- hold.stock[, proportion := Dsmvosd / sum(Dsmvosd), keyby = .(types, year, sem)]

# monthly return
load("stock.RData")
stock.m <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, month := month(date)
	][quarter == 1 | quarter == 2, sem := 1
	][quarter == 3 | quarter == 4, sem := 2
	][, Dretwd2 := Dretwd + 1
	][, .(month_ret = prod(Dretwd2, na.rm = TRUE) - 1), keyby = .(stock.id, year, sem, month)]

hold.ret <- hold.stock[stock.m, on = .(stock.id, year, sem)
	][, .(ret = sum(proportion * month_ret)), keyby = .(types, year, month)]

four <- fread("C:/Users/shenfan/Desktop/课题/俞老师自科/three_four_five monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)

# hold.ret <- four[hold.ret, on = .(year, month), nomatch = 0
#	][, rit := ret - rf
#	][, .(alpha_5 = coef(lm(rit ~ mkt_rf + smb + hml + rmw + cma))[1]), keyby = .(types)]

hold.ret <- four[hold.ret, on = .(year, month), nomatch = 0
	][, rit := ret - rf]


sub <- hold.ret[types == "same"]
same.reg <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, sub)

sub <- hold.ret[types == "social.only"]
social.reg <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, sub)

sub <- hold.ret[types == "fund.only"]
fund.reg <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, sub)

sub <- hold.ret[types == "nothing"]
nothing.reg <- lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, sub)

stargazer(same.reg, social.reg, fund.reg, nothing.reg, type = "html", out = "C:/Users/shenfan/Desktop/社保/same.doc")


## 累计收益率作图
# social.only后面部分月份都不存在？
SJ <- hold.ret[, .(year, month, types, ret)
	][, ret2 := ret + 1
	][year > 2003
	][, tag := (year - 2004) * 12 + month]


reg.roll <- list()
for (i in 2:168) {
	reg.roll[[i]] <- SJ[tag <= i, {
		I <- prod(ret2) %>% as.list()
	},
	keyby = .(types)]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:168, reg.roll)
roll <- roll[2:168]

reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]

SJ <- reg.cof[SJ, on = .(tag, types)]

date <- read_excel("C:/Users/shenfan/Desktop/社保/data/时间.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)]

SJ <- date[SJ, on = .(year, month)]
setnames(SJ,"V1","accumulated_return")

ggplot(SJ, aes(x = date)) +
	geom_line(aes(y = accumulated_return, linetype = types, color = types))
