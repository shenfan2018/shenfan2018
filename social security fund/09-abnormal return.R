# save(social.ret, file = "social_ret.RData") social ret
#社保重仓
social <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金/社保基金重仓流通股.xlsx")
social <- as.data.table(social)
setnames(social, 1:13, c("code", "name", "fund", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous", "date"))

social <- social[, stock.id := substring(code, 1, 6)
	][, c("stock.id", "name", "fund", "date", "hold.current", "hold.change", "hold.previous", "MV.current", "MV.change", "MV.previous", "proportion.current", "proportion.change", "proportion.previous")
	][, date := as.Date(date)
	][, year := year(date)
	][, quarter := quarter(date)]

social <- social[, proportion := MV.current / sum(MV.current, na.rm = TRUE), keyby = .(fund, year, quarter)]

# stock ,这里滞后了一期的
load("stock.RData")
stock.d <- stock[order(stock.id, date)
	][, year := year(date)
	][, quarter := quarter(date)
	][, quarter := ifelse(quarter == 1, 4, quarter - 1)
	][, year := ifelse(quarter == 4, year - 1, year)]

# 目前ret
social.ret <- stock.d[social, on = .(stock.id, year, quarter)
	][, .(ret = sum(proportion * Dretwd, na.rm = TRUE)), keyby = .(fund, date)]

# fund-ret
#基金的daily return
load("fund-NAV.RData")
fund <- data.NAV[, .(id, date, AdjustedNAVGrowth)]
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
fund <- fund.category[fund, on = .(id), nomatch = 0]

#选择混合型和股票型
fund <- fund[Category == "股票型基金" | Category == "混合型基金", .SD
	][category2 == "偏股混合型基金" | category2 == "普通股票型基金", .SD]

fund <- fund[, .(id, date, AdjustedNAVGrowth)]


# 选择相同的family
social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "fund.company"))
social.m <- social.m[, fund.company := str_replace_all(fund.company, "管理有限公司", "")
	][!is.na(fund.company), .SD]

social.only<-social.m[, .N, keyby = .(fund.company)]

fund.co <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人.xlsx")
fund.co <- as.data.table(fund.co)
setnames(fund.co, 1:3, c("code", "name", "fund.company"))
fund.co <- fund.co[, id := substring(code, 1, 6)
	][, c("id", "fund.company")]

fund.only <- fund.co[, .N, keyby = .(fund.company)]

same.family <- fund.only[social.only, on = .(fund.company), nomatch = 0
	][, .(fund.company)]

# social 选基金 选时间
social.f <- same.family[social.m, on = .(fund.company), nomatch = 0]
social.pur <- social.f[social.ret, on = .(fund), nomatch = 0
	][, year := year(date)
	][, quarter := quarter(date)
	][year == 2015
	][quarter == 2 | quarter == 3]

# fund 选基金 选时间
fund.f <- same.family[fund.co, on = .(fund.company), nomatch = 0]
fund.pur <- fund.f[fund, on = .(id), nomatch = 0
	][, year := year(date)
	][, quarter := quarter(date)
	][year == 2015
	][quarter == 2 | quarter == 3]

# in panel A we test two periods，t-1（2015，4/5/6）t （2015,7,8,9）for purchasing

t.test(social.pur[quarter == 2, "ret"])

t.test(social.pur[quarter == 3, "ret"])

t.test(social.pur[quarter == 3, "ret"], social.pur[quarter == 2, "ret"], paired = FALSE)

t.test(fund.pur[quarter == 2, "AdjustedNAVGrowth"])

t.test(fund.pur[quarter == 3, "AdjustedNAVGrowth"])

t.test(fund.pur[quarter == 3, "AdjustedNAVGrowth"], fund.pur[quarter == 2, "AdjustedNAVGrowth"], paired = FALSE)

# match factor
# 各种因子以及alpha
four <- fread("C://Users//shenfan//Desktop//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four <- as.data.table(four)
four <- four[, date := as.Date(trddy)]

social.four <- social.pur[, .(ret = mean(ret)), keyby = .(date)]
social.four <- four[social.four, on = .(date)]
social.four <- social.four[, quarter := quarter(date)
	][, rit := ret - rf]

lm(rit ~ mkt_rf + smb + hml, social.four[quarter == 2]) %>% summary()
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, social.four[quarter == 2]) %>% summary()
lm(rit ~ mkt_rf + smb + hml, social.four[quarter == 3]) %>% summary()
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, social.four[quarter == 3]) %>% summary()


fund.four <- fund.pur[, .(ret = mean(AdjustedNAVGrowth)), keyby = .(date)]
fund.four <- four[fund.four, on = .(date)]
fund.four <- fund.four[, quarter := quarter(date)
	][, rit := ret - rf]

lm(rit ~ mkt_rf + smb + hml, fund.four[quarter == 2]) %>% summary()
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, fund.four[quarter == 2]) %>% summary()
lm(rit ~ mkt_rf + smb + hml, fund.four[quarter == 3]) %>% summary()
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, fund.four[quarter == 3]) %>% summary()

# ret为fund i.ret为social
dif <- social.four[fund.four, on = .(date)
	][, rit := i.ret - ret]

lm(rit ~ mkt_rf + smb + hml, dif[quarter == 2]) %>% summary()
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, dif[quarter == 2]) %>% summary()
lm(rit ~ mkt_rf + smb + hml, dif[quarter == 3]) %>% summary()
lm(rit ~ mkt_rf + smb + hml + rmw + cma + umd, dif[quarter == 3]) %>% summary()



