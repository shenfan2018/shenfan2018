national <- read_excel("C:/Users/shenfan/Desktop/社保/data/国家队重仓流通股.xlsx")
national <- as.data.table(national)
setnames(national, 1:14, c("code", "stock.name", "national.name", "date", "realtime", "hold.T", "changes", "hold.T-1", "mv.T", "mv.change", "mv.T-1", 'pro.T', 'pro.change', 'pro.ST')) # pro.ST 上期

# 2015 二季度到三季度
national <- national[, date := as.Date(date)
	][, stock.id := substring(code, 1, 6)
	][, .(stock.id, date, national.name, pro.T, pro.change, pro.ST)]

# sum得
a <- national[, .(pro = sum(pro.T)), keyby = .(stock.id, date)
	][date > as.Date('2015-03-31') & date < as.Date('2017-09-30')]

# 扩
date <- a[order(date)
	][, .(date = unique(date))]
date <- date[["date"]]

stockid <- a[order(stock.id)
	][, .(stock.id = unique(stock.id))]
stockid <- stockid[["stock.id"]]

IO.national <- CJ(stock.id = stockid, date = date)

# match
IO.national <- a[IO.national, on = .(stock.id, date)]

# 处理
IO.national <- IO.national[, pro.2 := shift(pro, n = 1, fill = NA, type = 'lead'), keyby = .(stock.id)
#	][, national.buy := ifelse(is.na(pro.2) & !is.na(pro), 1, 0)  # sell
	][, national.buy := ifelse(is.na(pro) & !is.na(pro.2), 1, 0)   # buy
	][, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, date, year, quarter, national.buy)]

################################################## add 02-table3 add pre post
# quarter uel and buy
# add uel quarterly
uel.m <- fread('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/uel_month1.csv')
uel.m <- uel.m[, stkcd := as.character(stkcd)
	][, stock.id := str_pad(stkcd, 6, side = "left", pad = "0")
	][, quarter := ifelse(month == 1 | month == 2 | month == 3, 1, ifelse(month == 4 | month == 5 | month == 6, 2, ifelse(month == 7 | month == 8 | month == 9, 3, 4)))
	][, .(uel.m = mean(UEL)), keyby = .(stock.id, year, quarter)
	][, uel.m.2 := shift(uel.m, n = 1, fill = NA, type = 'lead'), keyby = .(stock.id)
	][, uel.dif := uel.m.2 - uel.m]

###########
# whether buy or not
load('IOquarter.RData')

IO <- IO[, .(stock.id, date, IOS, IOF, IOI, IOQ, sum)]
## add
IO <- IO[, lapply(colnames(IO[, 3:7]), str_c, ".shift") %>% unlist() := lapply(.SD[, 2:6], shift, n = 1, fill = NA, type = 'lag'), keyby = .(stock.id)
	][, IOS.buy := ifelse(IOS > IOS.shift, 1, 0)
	][, IOF.buy := ifelse(IOF > IOF.shift, 1, 0)
	][, IOI.buy := ifelse(IOI > IOI.shift, 1, 0)
	][, IOQ.buy := ifelse(IOQ > IOQ.shift, 1, 0)
	][, sum.buy := ifelse(sum > sum.shift, 1, 0)]

IO <- IO[, year := year(date)
	][, quarter := quarter(date)
	][, .(stock.id, year, quarter, IOS.buy, IOF.buy, IOI.buy, IOQ.buy, sum.buy)]

# match national
IO <- IO.national[IO, on = .(stock.id, year, quarter)]

# 仅选择2015.2到2017.2
IO <- IO[!is.na(date)]

# match
data <- uel.m[IO, on = .(stock.id, year, quarter), nomatch = 0]

table = melt(data, id.vars = 1:6, measure.vars = 8:13)

table <- table[, value2 := shift(value, n = 1, fill = NA, type = 'lead'), keyby = .(variable, stock.id)
	][, choose := ifelse(value == 0 & value2 == 1, 1, 0)
	][!is.na(choose) & !is.na(uel.m) & !is.na(uel.dif)
	][, value2 := NULL
	#	][order(variable, year, quarter, uel.m)
	][, vartag := ifelse(variable == 'IOS.buy', 1, ifelse(variable == 'IOF.buy', 2, ifelse(variable == 'IOI.buy', 3, ifelse(variable == 'IOQ.buy', 4, 5))))
	][, tag := (vartag - 1) * 53 + (year - 2004) * 4 + quarter]

# 如果该时间段，该组别都是0，没有任何加仓，要删掉 such as tag==2015
table <- table[, sum := sum(choose), keyby = .(tag)
	][!sum == 0]

# IOF.buy and sum.buy > 0.5, 匹配不了，要删除 
table <- table[, all := .N, keyby = .(tag)
	][, ratio := sum / all
	][ratio < 0.5]

# 用来match, such as 55，出现断层，没办法跑
Mat <- table[, .(tag = unique(tag))
	][, seq := seq(from = 1, to = (.N))]

# match
table <- Mat[table, on = .(tag)
	][, tag := seq]

# PSM
library(MatchIt)
roll <- list()
for (i in 1:max(table[, tag])) {
	roll[[i]] <- matchit(choose ~ uel.m, table[tag == i], method = "nearest", ratio = 1) %>% match.data() %>% as.data.table()
}
syf <- rbindlist(roll, fill = T)

# difference 的 difference
syf <- syf[order(variable, year, quarter, uel.m)
	][, tag2 := rep(1:(.N), each = 2), keyby = .(variable, year, quarter)
	][order(variable, year, quarter, tag2, - choose)
	][, uel.dif.2 := shift(uel.dif, n = 1, fill = NA, type = 'lead'), keyby = .(variable, year, quarter)
	][choose == 1
	][, dif := uel.dif - uel.dif.2
	][, .(t = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 3], dif = t.test(dif)$estimate, t2 = t.test(dif)$statistic, p = coeftest(lm(dif ~ 1), vcov. = NeweyWest)[, 4]), keyby = .(year, quarter, variable)]

write.csv(syf, "C://Users//shenfan//Desktop//mydatam.csv")

# buy and not buy
a <- syf[, .(uel.m = mean(uel.m), uel.m.2 = mean(uel.m.2), t = coeftest(lm(uel.dif ~ 1), vcov. = NeweyWest)[, 3], dif = t.test(uel.dif)$estimate, t2 = t.test(uel.dif)$statistic), keyby = .(year, quarter, choose, variable)]

write.csv(syf, "C://Users//shenfan//Desktop//mydatam.csv")



