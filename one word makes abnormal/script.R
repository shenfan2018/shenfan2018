# clean
load("variables.RData")

write.csv(data, "C://Users//shenfan//Desktop//one word.csv")

# date
date <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//date.xlsx")
date <- as.data.table(date)
date <- date[, date := as.Date(date)
	][, year := year(date)
	][, month := month(date)]

data <- date[data, on = .(year, month)
	][, dum.name := ifelse(num.name == 3, 1, 0)]



pmg(ret ~  log(size) , data, index = c("stock.id", "date")) %>% summary()

pmg(ret ~ dum.name, data, index = c("stock.id", "date")) %>% summary()

pmg(ret ~ dum.name + pri + size + BM + GP + INV + ret10 + vol + amihud + trd + float + IO + ANA + EMP + SH, data, index = c("stock.id", "date")) %>% summary()

plm(ret ~ pri + size + BM + GP + INV + ret10 + vol + amihud + trd + float + IO + ANA + EMP + SH, clean, model = "within", effect = "twoways", index = c("stock.id", "date")) %>% summary()
