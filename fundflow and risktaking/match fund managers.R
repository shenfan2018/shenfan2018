load("datamain5.RData")

#匹配基金经理
fund.manager <- read_excel("C://Users//shenfan//Desktop//data//处理理清//1 构造格式.xlsx", sheet = "日期更改")
fund.manager <- as.data.table(fund.manager)
setnames(fund.manager, "date", "DateQ")
fund.manager <- fund.manager[, DateQ := as.Date(DateQ)]

#匹配基金经理个人特征
fund.manager.ch <- read_excel("C://Users//shenfan//Desktop//data//Fund_FundManager.xlsx")
fund.manager.ch <- as.data.table(fund.manager.ch)
fund.manager.ch <- fund.manager.ch[, c("MasterFundCode", "FullName", "Gender", "BirthDate", "Degree", "BusinessDuration", "Resume")]
setnames(fund.manager.ch, c("MasterFundCode", "FullName"), c("id", "manager"))
#fund.manager.ch原始数据有问题，存在重复项
fund.manager.ch <- fund.manager.ch[, yesno := (duplicated(fund.manager.ch, by = c("id", "manager")))
	][yesno == FALSE, .SD]

#这里开始,先匹配基金经理
data <- data.main5
data <- data[fund.manager, on = .(code, DateQ), nomatch = 0
	][value == "TRUE", .SD]

# 一个基金同一个时间几个基金经理
data <- data[, group := .N, keyby = .(id, DateQ)
	][group == 1, .SD] #这里只把等于1的给取出来
data <- fund.manager.ch[data, on = .(id, manager), nomatch = 0]


#group的，也就是group不选等于1，这里的操作上面的[group == 1, .SD]别运行 专门来看group，团队对基金risk的影响的
data <- data[, yesno := duplicated(data, by = c("id", "DateQ"))
	][yesno == FALSE, .SD
	][, group := ifelse(group == 1, 0, 1)] #group=0是一个人 否则是团体

data.main7 <- data
save(data.main7, file = "datamain7.RData")


#男为1，女为0
data <- data[, Gender := ifelse(Gender == "男", 1, 0)
	][, master := ifelse(Degree == "硕士研究生" | Degree == "MBA/EMBA", 1, 0)
	][, PHD := ifelse(Degree == "博士研究生", 1, 0)
	][, undergraduate := ifelse(Degree == "本科" | Degree == "大专", 1, 0)]

#风格的不同有一定的不同
data <- data[, st.mixed := ifelse(style == "mixed", 1, 0)
	][, st.value := ifelse(style == "value", 1, 0)
	][, st.growth := ifelse(style == "growth", 1, 0)]

data.main6 <- data

save(data.main6, file = "datamain6.RData")

