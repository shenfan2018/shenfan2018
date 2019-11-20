# 成为社保管理资格的
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/socialpf.RData")
load("C:/Users/shenfan/source/repos/shenfan2018/social security fund new/socialchar.RData")

social <- social[order(fund.company, date)
	][, .SD[1], keyby = .(fund.company)
	][order(date)
	][, .(fund.company, date)]

# 就取2010之后的,大成基金、富国基金、工银瑞信基金、广发基金、海富通基金、汇添富基金、银华基金
event <- read_excel("C:/Users/shenfan/Desktop/社保/data/event.xlsx")




