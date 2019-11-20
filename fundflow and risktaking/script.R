#InvestmentStyle    N
#1:价值优化型 500
#2:价值型 124
#3:价值投资型 756

#4:分红型 32
#5:增值型 2905
#6:增长型 10
#7:平衡型 559
#8:成长型 2487
#9:指数型 92
#10:收益型 1031
#11:混合收益型 32
#12:积极成长型 1260
#13:稳健型 14
#14:稳健增值型 115
#15:稳健增长型 32
#16:稳健成长型 4637
#17:稳定增长型 32
#18:稳定成长型 422

data[InvestmentStyle == "价值优化型" | InvestmentStyle == "价值优" | InvestmentStyle == "价值投资型" | InvestmentStyle == "价值优化型", style := 1
	][InvestmentStyle = "增值型"]

a3 <- a2[, lapply(colnames(a2[, 3:8]), str_c, ".L") %>% unlist() := lapply(.SD[, 3:8], shift, n = 1L, type = "lead")]

data <- data.main5
style <- read_excel("C://Users//shenfan//Desktop//data//股票型混合型//style.xlsx")
style <- as.data.table(style)
setnames(style, 1:3, c("code", "name", "style"))
style<-style[,name:=NULL]
data <- style[data, on = .(code)]
data <- data[id == "001622", style := "平衡风格型基金"
	][style == "平衡风格型基金", style := "mixed"
	][style == "价值风格型基金", style := "value"
	][style == "成长风格型基金", style := "growth"]

a <- data[, .N, by = style]

load("datamain7.RData")
data <- data.main7

data <- data.main7[group == 1, .SD]
summary(plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))


