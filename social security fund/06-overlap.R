#社保重仓
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)
	][quarter == 2 | quarter == 4
	][, sem := ifelse(quarter == 2, 1, 2)]

social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "company"))
social.m <- social.m[, company := str_replace_all(company, "管理有限公司", "")
	][!is.na(company), .SD]

#############################################################新一轮
load("socialpf.RData")
setnames(social, "fund.company", "company")
# 只要名单就可以了好像
social.pf <- social[quarter == 2 | quarter == 4
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(fund, company, date, year, sem, stock.id, MV.current)
	][, .(stock.id = unique(stock.id)), keyby = .(company, year, sem)
	][, social.buy := 1]

# 基金portfolio
load("portfolio.RData")

# 整理
fund.pf <- portfolio[, year := year(date)
	][, quarter := quarter(date)
	][, sem := ifelse(quarter == 2, 1, 2)
	][, .(id, date, year, sem, stock.id, proportion.stock)]

fund.co <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人.xlsx")
fund.co <- as.data.table(fund.co)
setnames(fund.co, 1:3, c("code", "name", "company"))
fund.co <- fund.co[, id := substring(code, 1, 6)
	][, c("id", "company")]

fund.pf <- fund.co[fund.pf, on = .(id)]

# overlap
overlap <- social.pf[fund.pf, on = .(company, year, sem, stock.id)
	][is.na(social.buy), social.buy := 0
	][, .(overlap1 = sum(social.buy) / (.N), overlap2 = sum(proportion.stock * social.buy) / sum(proportion.stock)), keyby = .(id, year, sem)]

# 删除其他类型的基金？
#fund style(wind+csmar)
fund.csmar <- read_excel("C://Users//shenfan//Desktop//基金经理//data//基金类型风格//Fund_MainInfo.xlsx")
fund.csmar <- as.data.table(fund.csmar)
setnames(fund.csmar, 1:2, c("id", "name"))
fund.csmar <- fund.csmar[name != "基金全称'", .SD
	][name != "没有单位'", .SD
	][, name := NULL]

fund.wind <- read_excel("C://Users//shenfan//Desktop//基金经理//data//基金类型风格//windstyle.xlsx")
fund.wind <- as.data.table(fund.wind)
setnames(fund.wind, 1:6, c("code", "name", "category2", "category1", "style1", "style2"))
fund.wind <- fund.wind[, id := substring(code, 1, 6)
	][, c("style1", "style2", "name", "category1") := NULL]
fund.category <- fund.csmar[fund.wind, on = .(id)]
#match
overlap <- fund.category[overlap, on = .(id), nomatch = 0]

#选择混合型和股票型
overlap <- overlap[Category == "股票型基金" | Category == "混合型基金", .SD
	][category2 == "偏股混合型基金" | category2 == "普通股票型基金", .SD]

overlap <- overlap[, .(id, year, sem, overlap1, overlap2)]



