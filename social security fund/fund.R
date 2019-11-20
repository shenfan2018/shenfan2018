# 基金类
fund1 <- read_excel("C:/Users/shenfan/Desktop/社保/data/全部持股(明细) 偏股混合型 半年.xlsx")
fund2 <- read_excel("C:/Users/shenfan/Desktop/社保/data/全部持股(明细) 普通股票型 半年.xlsx")
fund <- rbindlist(list(fund1, fund2))
rm(fund1,fund2)
fund <- as.data.table(fund)

plm(IO.social ~ turnover + ret, data, model = "within", effect = "twoways", index = c("id", "DateQ")) %>% summary()