name <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//¹ÉÆ±¸üÃû.xlsx")
name <- as.data.table(name)
setnames(name, 4:5, c("name.before", "name.after"))
name <- name[, ST := str_extract(name.before, "ST")
	][is.na(ST)
	][, G := str_extract(name.before, "G")
	][is.na(G)
	][, S := str_extract(name.before, "S")
	][is.na(S)
	][, ST := str_extract(name.after, "ST")
	][is.na(ST)
	][, G := str_extract(name.after, "G")
	][is.na(G)
	][, S := str_extract(name.after, "S")
	][is.na(S)
	][, - (6:11)]





