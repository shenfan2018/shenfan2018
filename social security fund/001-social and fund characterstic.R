# 社保 size
load("socialpf.RData")
social <- social[, .(social_size = sum(MV.current, na.rm = TRUE)), keyby = .(fund, date)]

social_age <- social[, .SD[1], keyby = .(fund)
	][, social_date := date
	][, .(fund, social_date)]

social.char <- social_age[social, on = .(fund)
	][, days := difftime(date, social_date, units = c("days"))
	][, social_age := as.numeric(days) / 365 + 0.5
	][, .(fund, date, social_size, social_age)]

# add social company
# 匹配fund family
social.m <- read_excel("C:/Users/shenfan/Desktop/社保/data/社保基金管理人.xlsx")
social.m <- as.data.table(social.m)
setnames(social.m, 1:2, c("fund", "socialcompany"))
social.m <- social.m[, socialcompany := str_replace_all(socialcompany, "管理有限公司", "")
	][!is.na(socialcompany), .SD]
social.char <- social.m[social.char, on = .(fund)]

save(social.char, file = 'socialchar.RData')

###############################################################
# mutual fund size age
load("fund1.RData")

# match 基金公司 简称
fundcompany <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金管理人.xlsx")
fundcompany <- as.data.table(fundcompany)
setnames(fundcompany, 1:3, c("code", "name", "company"))
# 全称
fundcompany.2 <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金与基金管理公司.xlsx")
fundcompany.2 <- as.data.table(fundcompany.2)
setnames(fundcompany.2, 1:3, c("code", "name", "company.q"))
# match
fundcompany <- fundcompany.2[fundcompany, on = .(code, name)]

# 基金公司信息
company <- read_excel("C:/Users/shenfan/Desktop/社保/data/基金公司规模.xlsx")
company <- as.data.table(company)
setnames(company, 1:3, c("company.q", "num", "company_size"))
setnames(company, 17, "startdate")
company <- company[, .(company.q, company_size, startdate)
	][, startdate := as.Date(startdate)]

# match
fundcompany <- company[fundcompany, on = .(company.q)]

# match fund character
fund.char <- fundcompany[fund.1, on = .(code)
	][, days := difftime(DateQ, startdate, units = c("days"))
	][, company_age := as.numeric(days) / 365
	][, date := DateQ
	][, .(code, id, name, company, company.q, date, fund_age, fund_size, company_size, company_age, Category, InvestmentStyle, category2, inflow, outflow, netflow)]

save(fund.char, file = 'fundchar.RData')


#################################################################
# 加入monthly volatility greater than top 10%/20%
# 各种因子以及alpha
four.d <- fread("C://Users//shenfan//Desktop//基金经理//data//Carhat four factors//央财//three_four_five_factor_daily//fivefactor_daily.csv")
four.d <- as.data.table(four.d)
four.d <- four.d[, date := as.Date(trddy)
	][, year := year(date)
	][, month := month(date)
	][, mkt := mkt_rf + rf
	][, .(vol = sd(mkt)), keyby = .(year, month)
	][year < 2018 & year > 2003
	][-(1:3), .SD]

## bear/bull market (monthly)
# bull market when the month's past 12 month cumulative market risk premium is positive
four <- fread("C:/Users/shenfan/Desktop/wechat//Buffett alpha/2019-04-01-three_four_five_factor_monthly/three_four_five_factor_monthly/fivefactor_monthly.csv")
four <- as.data.table(four)
bb <- four[, .(year, month, mkt_rf)]

#先变月度
bb <- bb[, tag := seq(1:(.N))
	][, mkt_rf2 := mkt_rf + 1]

reg.roll <- list()
for (i in 13:299) {
	reg.roll[[i]] <- bb[tag >= i - 12 & tag <= i - 1, {
		I <- prod(mkt_rf2, na.rm = TRUE) %>% as.list()
	}]
	rbindlist(reg.roll)
}

roll <- data.table(tag = 1:299, reg.roll)
roll <- roll[13:299]
reg.cof <- roll[, rbindlist(.SD[['reg.roll']]), by = tag]
bb <- reg.cof[bb, on = .(tag)
	][, bull := ifelse(V1 > 1, 1, 0)
	][year > 2003
	][, .(year, month, bull)]

# match
# add vol
monthlyfactor <- four.d[four, on = .(year, month)]
# add bull bear
monthlyfactor <- bb[monthlyfactor, on = .(year, month)]

save(monthlyfactor, file = "monthlyfactor.RData")


############################################################# # social get style from fund, accroding from overlap 
load("overlap.RData")

# fund char
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/fundchar.RData")
style <- fund.char[, .(id, date, style)]

# match
overlap <- style[overlap, on = .(id, date)
	][order(fund, date, - overlap2, - overlap1)
	][!is.na(style)
	][, .SD[1], keyby = .(fund, date)]



################################### fund family characterstic
# company flow most important
# 添加市值


# ret
load("fund_ret.RData")
fund.ret <- fund.ret[, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(ret.f = prod(1 + ret.f.raw) - 1), keyby = .(id, year, quarter)]

data <- marketvalue[fund.ret, on = .(id, year, quarter)]

