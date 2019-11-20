# concering 36-rank
# 仅用社保
load("social_ret.RData")
load("socialper.RData")
load("socialchar.RData")

social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)]

social <- social.char[social.ret, on = .(fund, year, quarter)]

social <- socialper[social, on = .(fund, year, month)
	][, logsocial_size := log(social_size * 10000)
	][, .(fund, social_company, social_style2, date, year, month, ret.s, ret.s_mkt, alpha_four, logsocial_age, logsocial_size)]

setnames(social, "social_company", "company")

# company
load("companychar.RData")
social <- company.char[social, on = .(company, date)]

# 滞后
for (i in 1:41) {
	social[, str_c("alpha_four.", i) := shift(alpha_four, n = i, fill = NA, type = "lag"), keyby = .(fund)]
}

for (i in 1:41) {
	social[, str_c("ret.s.", i) := shift(ret.s, n = i, fill = NA, type = "lag"), keyby = .(fund)]
}

for (i in 1:41) {
	social[, str_c("ret.s_mkt.", i) := shift(ret.s_mkt, n = i, fill = NA, type = "lag"), keyby = .(fund)]
}

write.csv(social, "C://Users//shenfan//Desktop//rank36.csv")

social <- social[month == 12]

felm(ret.s_mkt ~ ret.s_mkt.31 + ret.s_mkt.32 + ret.s_mkt.33 + ret.s_mkt.34 + ret.s_mkt.35 + ret.s_mkt.36 + ret.s_mkt.37 + ret.s_mkt.38 + ret.s_mkt.39 + ret.s_mkt.40 + ret.s_mkt.41 + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + social_style2 + fund, social) %>% summary()

felm(ret.s ~ ret.s.31 + ret.s.32 + ret.s.33 + ret.s.34 + ret.s.35 + ret.s.36 + ret.s.37 + ret.s.38 + ret.s.39 + ret.s.40 + ret.s.41 + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + social_style2 + company, social) %>% summary()


felm(alpha_four ~ alpha_four.1 + alpha_four.2 + alpha_four.3 + alpha_four.4 + alpha_four.5 + alpha_four.6 + alpha_four.7 + alpha_four.8 + alpha_four.9 + alpha_four.10 + alpha_four.11 + alpha_four.12 + alpha_four.13 + alpha_four.14 + alpha_four.15 + alpha_four.16 + alpha_four.17 + alpha_four.18 + alpha_four.19 + alpha_four.20 + alpha_four.21 + alpha_four.22 + alpha_four.23 + alpha_four.24 + alpha_four.25 + alpha_four.26 + alpha_four.27 + alpha_four.28 + alpha_four.29 + alpha_four.30 + alpha_four.31 + alpha_four.32 + alpha_four.33 + alpha_four.34 + alpha_four.35 + alpha_four.36 + alpha_four.37 + alpha_four.38 + alpha_four.39 + alpha_four.40 + alpha_four.41 + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + social_style2 + company, social) %>% summary()


felm(alpha_four ~ alpha_four.31 + alpha_four.32 + alpha_four.33 + alpha_four.34 + alpha_four.35 + alpha_four.36 + alpha_four.37 + alpha_four.38 + alpha_four.39 + alpha_four.40 + alpha_four.41 + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + company, social) %>% summary()

felm(alpha_four ~ alpha_four.8 + alpha_four.9 + alpha_four.10 + alpha_four.11 + alpha_four.12 + alpha_four.13 + alpha_four.15 + alpha_four.15 + alpha_four.16 + alpha_four.7 + alpha_four.6 + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + social_style2 + fund, social) %>% summary()




reg1 <- felm(alpha_four ~ alpha_four.31 + alpha_four.32 + alpha_four.33 + alpha_four.34 + alpha_four.35 + alpha_four.36 + alpha_four.37 + alpha_four.38 + alpha_four.39 + alpha_four.40 + alpha_four.41 + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + social_style2 + fund, social)

reg2 <- felm(alpha_four ~ alpha_four.31 + alpha_four.32 + alpha_four.33 + alpha_four.34 + alpha_four.35 + alpha_four.36 + alpha_four.37 + alpha_four.38 + alpha_four.39 + alpha_four.40 + alpha_four.41 + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + social_style2 + company, social)

stargazer(reg1, reg2, type = "html", out = "C:/Users/shenfan/Desktop/rank36.doc", report = ('vc*t'))


#################################################################### 第二种 all sample 用social*ret
load("fund_ret.RData")
load("fundchar.RData")

fund.char <- fund.char[, year := year(date)
	][, quarter := quarter(date)
	][, fund_category := NULL]

fund <- fund.char[fund.ret, on = .(id, year, quarter)]

# 删除company里面基金少于4个
fund <- fund[, N := (.N), keyby = .(year, month, company)
	][N > 3]

fund <- fund[order(id, year, month)
	][, .(id, company, date, year, month, quarter, fund_style, fund_size, fund_age, ret.f, ret.f_mkt)]

setnames(fund, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# social
load("social_ret.RData")
load("socialchar.RData")
social.char <- social.char[, year := year(date)
	][, quarter := quarter(date)]

social <- social.char[social.ret, on = .(fund, year, quarter)]
# social id 
socialid <- social[order(fund)
	][, .SD[1], keyby = .(fund)
	][, id := 900001:900044
	][, .(fund, id)]

# match
social <- socialid[social, on = .(fund)
	][, logsocial_size := log(social_size * 10000)
	][, .(id, social_company, date, year, month, quarter, social_style2, social_size, social_age, ret.s, ret.s_mkt)]

setnames(social, 2, c("company"))
setnames(social, 7:11, c("style", "size", "age", "ret", "ret_mkt"))

# match rbindlist
data <- rbindlist(list(fund, social))

# social
data <- data[, social := ifelse(id > "900000", 1, 0)]

# company
load("companychar.RData")
data <- company.char[data, on = .(company, date)]

## 滞后
#for (i in 31:41) {
	#social[, str_c("alpha_four.", i) := shift(alpha_four, n = i, fill = NA, type = "lag"), keyby = .(id)]
#}

for (i in 31:41) {
	data[, str_c("ret.", i) := shift(ret, n = i, fill = NA, type = "lag"), keyby = .(id)]
}

for (i in 31:41) {
	data[, str_c("ret_mkt.", i) := shift(ret_mkt, n = i, fill = NA, type = "lag"), keyby = .(id)]
}

#write.csv(social, "C://Users//shenfan//Desktop//rank36.csv")

data <- data[, log_size := log(size)
	][, log_age := log(age)]

felm(ret_mkt ~ social * ret_mkt.31 + social * ret_mkt.32 + social * ret_mkt.33 + social * ret_mkt.34 + social * ret_mkt.35 + social * ret_mkt.36 + social * ret_mkt.37 + social * ret_mkt.38 + social * ret_mkt.39 + social * ret_mkt.40 + social * ret_mkt.41 + log_size + log_age + logcompany_size + logcompany_age | year + style + id, data) %>% summary()


felm(ret.s ~ ret.s.31 + ret.s.32 + ret.s.33 + ret.s.34 + ret.s.35 + ret.s.36 + ret.s.37 + ret.s.38 + ret.s.39 + ret.s.40 + ret.s.41 + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + social_style2 + company, social) %>% summary()


felm(alpha_four ~ alpha_four.31 + alpha_four.32 + alpha_four.33 + alpha_four.34 + alpha_four.35 + alpha_four.36 + alpha_four.37 + alpha_four.38 + alpha_four.39 + alpha_four.40 + alpha_four.41 + logsocial_size + logsocial_age + logcompany_size + logcompany_age | year + social_style2 + fund, social) %>% summary()
