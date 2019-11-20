# flow
load("fundchar.RData")
load("fundper.RData")

data <- fund.char[, year := year(date)
	][, month := month(date)]

data <- fundper[data, on = .(id, year, month)]

# dummy social in family
load("socialchar.RData")
socialcompany <- social.char[, .(company = unique(socialcompany))
	][, dummysocial := 1]

data <- socialcompany[data, on = .(company)]

data <- data[, dummysocial := ifelse(is.na(dummysocial), 0, 1)]

# lagged netflow
data <- data[, lapply(colnames(data[, 22:24]), str_c, ".1") %>% unlist() := lapply(.SD[, 21:23], shift, n = 1, type = "lag", fill = NA), keyby = .(id)]

# duplicate couples
data <- data[, yesno := duplicated(netflow), keyby = .(id, date)
	][yesno == "FALSE"
	][, yesno := NULL]


write.csv(data, "C://Users//shenfan//Desktop//flowdummysocial.csv")

plm(netflow ~ dummysocial + netflow.1 + fund_age + fund_size + company_size + company_age, data, model = "within", effect = "twoways", index = c("id", "date")) %>% summary()


data[, felm(netflow ~ alpha_four + dummysocial + netflow.1 + fund_age + fund_size + company_size + company_age | id + date)] %>% summary()



