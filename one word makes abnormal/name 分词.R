## added  Ãû×ÖµÄcharacter 2019.9.23 ·Ö´Ê
## name
name <- read_excel("C://Users//shenfan//Desktop//one word makes abnormal profit//name.xlsx")
name <- as.data.table(name)
setnames(name, 1:3, c("code", "name", "name.b"))
name <- name[, stock.id := substring(code, 1, 6)
	][, .(stock.id, name)]

write.csv(name, "C://Users//shenfan//Desktop//aaaa.csv")

setwd("C:/Users/shenfan/Desktop")

name <- readtext(file = "aaaa.csv", text_field = "name")

name <- corpus(name)

c <- summary(name)

# ×Ö·û
d <- data.table(character = ntoken(name))


c <- summary(corpus_subset(name))

b <- tokens(name, what = c("word"), remove_punct = TRUE)
b <- as.list(b)


#b <- rbindlist(b, fill = TRUE)

#b <- data.table((unlist(b, recursive = TRUE, use.names = TRUE)))

b <- do.call(rbind, lapply(lapply(b, unlist), `length<-`, max(lengths(b)))) %>% as.data.table()

write.csv(b, "C://Users//shenfan//Desktop//cc.csv")


d <- dfm(name)
d <- as.data.table(d)


