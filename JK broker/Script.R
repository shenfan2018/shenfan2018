# read
lehit <- read_excel("C:/Users/shenfan/Desktop/YIFAN/LEHTI.xlsx", sheet = "OWN FIRM")
lehit <- as.data.table(lehit)
lehit <- lehit[, date := as.Date(Transaction_Date)]


linder <- fread("C:/Users/shenfan/Desktop/YIFAN/JOHAN_LINDER_TRANSACTIONS.csv", na.strings = getOption("NA"), sep = "auto", encoding = "UTF-8", fill = T)
linder <- linder[!is.na(Volume)]


# big euroclear_hh_co_1995_2014
a <- fread("C:/Users/shenfan/Desktop/YIFAN/euroclear_hh_co_1995_2014.csv")



