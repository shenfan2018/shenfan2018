#社保重仓
social<-read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
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

social <- social.m[social, on = .(fund)]


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

fund.co <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人.xlsx")
fund.co <- as.data.table(fund.co)
setnames(fund.co, 1:3, c("code", "name", "fund.company"))
fund.co <- fund.co[, id := substring(code, 1, 6)
	][, c("id", "fund.company")]

portfolio <- fund.co[portfolio, on = .(id)]

# 社保的unique的company
unq.co <- social.m[, .(fund.company = unique(fund.company))]
#公募基金unique的company
unq.co.f <- portfolio[, .(fund.company = unique(fund.company))]
#两者共同的company
unq.co <- unq.co[unq.co.f, on = .(fund.company), nomatch = 0]

sf.social <- unq.co[social, on = .(fund.company), nomatch = 0]
sf.fund <- unq.co[portfolio, on = .(fund.company), nomatch = 0]


















# 社保的unique的股票
# unq.stock <- social[!is.na(fund.company),.SD
#	][order(date, fund.company, stock.id)
#	][, yesno := duplicated(stock.id), keyby = .(date, fund.company)
#	][yesno == FALSE, .SD
#	][, c("fund.company", "date", "stock.id")
#	][, yesno := 1]


# 选出公募基金管理人是社保的，统计每只股票一个基金管理公司持有的数量
po.social <- unq.co[portfolio, on = .(fund.company), nomatch = 0
	][order(fund.company, date, stock.id)
	][, num := (.N), keyby = .(fund.company, date, stock.id)]

# 筛选出股票
fund.stock <- po.social[, yesno := duplicated(num), keyby = .(fund.company, date, stock.id)
	][yesno == FALSE, .SD
	][, c("fund.company", "date", "stock.id", "num")]

# 匹配到社保的股票里
a <- fund.stock[social, on = .(fund.company, date, stock.id)
	][order(fund.company, date, stock.id)]
# 删非基金公司，以及date
a <- unq.co[a, on = .(fund.company), nomatch = 0
	][quarter != 3 & quarter != 1, .SD
	][, buyornot := ifelse(is.na(num), 0, 1)]

# 统计整体的时间
date <- a[, .N, keyby = .(buyornot, date)
	][, all := sum(N), keyby = .(date)
	][buyornot == 1,.SD
	][, concentration := N / all
	][order(concentration)]

# 统计整体的公司
company <- a[, .N, keyby = .(buyornot, fund.company)
	][, all := sum(N), keyby = .(fund.company)
	][buyornot == 1,.SD
	][, concentration := N / all
	][order(concentration)]

# 统计整体时间和公司
b <- a[, .N, keyby = .(fund.company, date, buyornot)
	][, all := sum(N), keyby = .(fund.company, date)
	][buyornot == 1, .SD
	][, concentration := N / all
	][order(concentration)]

# 删除N<10的，然后最分散的20%，最集中的20%
b <- b[N > 10, .SD
	][, rank := ntile(concentration, 5)]

ggplot(date, aes(x = date, y = concentration)) + geom_point()
ggplot(b, aes(x = date, y = concentration, colour = fund.company)) + geom_point()

top.company <- b[rank == 5, .N, keyby = .(fund.company)]
top.date <- b[rank == 5, .N, keyby = .(date)]

bottom.company <- b[rank == 1, .N, keyby = .(fund.company)]
bottom.date <- b[rank == 1, .N, keyby = .(date)]



c <- social[fund.company == "华夏基金"
	][order(stock.id, date)]






# 填补清仓的吧
date <- CJ(date = seq(social[, min(date)], social[, max(date)], by = "quarter"), fund = unique(social$fund), id = unique(social$id))
date <- date[, year := year(date)
	][, quarter := quarter(date)]

social2 <- social[date, on = .(year, quarter, id, fund)
	][order(id, fund, year, quarter)
	][, hold.previous := shift(hold.current, n = 1, fill = NA, type = "lag"), keyby = .(id, fund)
	][!is.na(hold.current) | !is.na(hold.previous), .SD
	][, zero := ifelse(is.na(hold.previous), 1, 0)
	][zero == 1, hold.previous := 0
	][, zero := ifelse(is.na(hold.current), 1, 0)
	][zero == 1, hold.current := 0]


	][, mv.previous2 := shift(MV.previous, n = 1, fill = NA, type = "lag"), keyby = .(id, fund)
	][, proportion.previous2 := shift(proportion.previous, n = 1, fill = NA, type = "lag"), keyby = .(id, fund)]


a <- social[id == 600486]


b <- a[date, on = .(year, quarter, id, fund)]

b <- b[id == 600486
	][order(id, fund, year, quarter)]


c <- social[fund == "全国社保基金102组合" & date == "2017-12-31"]




country <- read_excel("C:/Users/shenfan/Desktop/社保/data/国家队重仓流通股.xlsx")
country <- as.data.table(country)



