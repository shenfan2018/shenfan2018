# sum holding
load("IOS.RData")

# industry
industry <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//name2/TRD_Co.xlsx")
industry <- as.data.table(industry)
setnames(industry, "Stkcd", "stock.id")
industry <- industry[stock.id != "证券代码'", .SD
	][stock.id != "没有单位'"
	][, .(stock.id, Nnindcd, Nnindnme)
	][, hy := substring(Nnindcd, 1, 1)]
                  
# match
data <- industry[IOS.m, on = .(stock.id)                       
	][, .(proportion = sum(proportion)), keyby = .(date, hy)
	][order(date, - proportion)
	][, .SD[1], keyby = .(date)]



