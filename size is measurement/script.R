# JK chinese
fund <- read_excel('C:/Users/shenfan/Desktop/size is measurement/ChinesefundID&names.xlsx')
fund <- as.data.table(fund)
fund <- fund[, fundcode := as.character(masterfundcode)
	][, fundcode := str_pad(fundcode, 6, side = "left", pad = "0")]

# wind
wind <- read_excel('C:/Users/shenfan/Desktop/size is measurement/基金类型.xlsx')
wind <- as.data.table(wind)
setnames(wind, 1:7, c('code', 'name', 'category1', 'category2', 'category1.e', 'category2.e','Foundation_Day'))

wind <- wind[, fundcode := substring(code, 1, 6)]

category <- fund[wind, on = .(fundcode)
	][, .(fundcode, fullname, name, category1, category2, category1.e, category2.e)]

write.csv(category, "C://Users//shenfan//Desktop//fundinfo.csv")


