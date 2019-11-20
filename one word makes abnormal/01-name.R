## 我的
load("C:/Users/shenfan/source/repos/shenfan2018/Buffett alpha/stock20030101-20190331.RData")
stock.p <- stock
stock.p <- stock.p[order(stock.id, date)
	][, Dretwd2 := Dretwd + 1
	][, month := month(date)
	][, year := year(date)
	][, .(ret = prod(Dretwd2, na.rm = TRUE) - 1), keyby = .(stock.id, year, month)]

## name
name <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//name.xlsx")
name <- as.data.table(name)
setnames(name, 1:3, c("code", "name", "name.b"))
name <- name[, stock.id := substring(code, 1, 6)]
#匹配行业
name2 <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//name2/TRD_Co.xlsx")
name2 <- as.data.table(name2)
setnames(name2, "Stkcd", "stock.id")
name2 <- name2[stock.id != "证券代码'", .SD
	][stock.id != "没有单位'"
	][, .(stock.id, Nnindcd, Nnindnme, Commnt, Listdt)]
# 综合wind和csmar 3568只股票
name <- name2[name, on = .(stock.id), nomatch = 0]
rm(name2)

## 开始剔除股票
# 剔除ST 3483只股票
name <- name[, ST := str_extract(name, "ST")
	][is.na(ST)
	][, ST := NULL]

# 剔除金融 J是金融 3391只股票
name <- name[, hy := substring(Nnindcd, 1, 1)
	][hy != "J"]

# 剔除AH股一同上市 3316只股票
name <- name[is.na(Commnt)
	][, Commnt := NULL]

# 剔除带字母的 3284只股票
name <- name[, zimu := str_extract(name, "A")
	][is.na(zimu)
	][, zimu := NULL]

# 剔除 港口股(首先行业要是G）3268只股票
name <- name[, port := str_extract(name, "..港")
	][is.na(port) | hy != "G"
	][, port := NULL]

# 剔除 2017-6-30以后发行的股票 2988只股票
name <- name[, Listdt := as.Date(Listdt)
	][Listdt < "2017-07-01"]

# 赋值
name <- name[, num.name := nchar(name)]

# 按照行业看有无三个字 还剩下2421只股票
hyyw <- name[hy == "C", hy := Nnindcd
	][, .(.N), keyby = .(hy, num.name)
	][, shifou := (.N), keyby = hy
	][shifou == 2, .(hy = unique(hy))]
#删除
name <- hyyw[name, on = .(hy), nomatch = 0]

# name匹配上stock
stock.p <- name[stock.p, on = .(stock.id), nomatch = 0]

# 选择2004.1-2018.6
stock.p <- stock.p[!year == 2003
	][!year == 2018 | month < 7
	][!year == 2019]
# 这里save一下
save(stock.p, file = "sample.RData")
# 256183

# 分词
fc <- read_excel("C:/Users/shenfan/Desktop/one word makes abnormal profit/name 分词.xlsx")
fc <- as.data.table(fc)
fc <- fc[, stock.id := as.character(stock.id)
	][, stock.id := str_pad(stock.id, 6, side = "left", pad = "0")]


## name的验证
a <- name
a <- a[, hy2 := substring(hy, 1, 1)]

a <- a[, num := .N, keyby = .(hy2)
	][order(num.name, hy2)
	][, num.2 := .N, keyby = .(num.name, hy2)
	][, yesno := duplicated(num.2), keyby = .(num.name, hy2)
	][yesno == FALSE
	][, .(num.name, hy2, num.2, num)
	][, ratio := num.2 / num]

write.csv(a, "C://Users//shenfan//Desktop//mydatam.csv")
