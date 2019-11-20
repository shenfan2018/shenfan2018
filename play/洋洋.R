data <- 1:16

data_random <- data.table(replicate(1e6, sample(data)))

random <- function(d) {

	x <- seq(from = 2, to = length(d), by = 2)

	b <- function(x) {

		sort(d[(x - 1):x])

	}

	sapply(seq(from = 2, to = length(d), by = 2), b, simplify = FALSE) %>% unlist()

}


data.smlt <- data_random[, lapply(.SD, random)] %>% melt(measure.vars = colnames(data_random))

data.smlt[, ':='(id.match = rep(1:8, each = 2)), by = .(variable)

	][, id.paste := str_c(value, collapse = ""), by = .(id.match, variable)

	][, id.paste := as.numeric(id.paste)

	][, id.group := prod(id.paste + 0.01), by = .(variable)]



# data.smlt[, uniqueN(id)]

setnames(data.smlt, 1:2, c("gnrt", "prtcpt"))

fwrite(data.smlt, "C://Users//shenfan//Desktop//data.smlt16.csv")



data.smlt<-fread("C://Users//shenfan//Desktop//yang//data.smlt16.csv")

yang <- read_excel("C://Users//shenfan//Desktop//yang//yangyang16.1.xlsx")
# yang <- fread("C://Users//shenfan//Desktop//yang//数据替换-0113 先前实验.csv")
yang <- as.data.table(yang)
setnames(yang, 1:4, c("id", "duration", "point", "income"))

data <- data.smlt
setnames(data, 2, "id")
data <- data[, .(gnrt = unique(gnrt))
	][, num := seq(1:(.N))
	][num < 654730] #654730
data.d <- data[data.smlt, on = .(gnrt), nomatch = 0]
data.d <- yang[data.d, on = .(id)]

data.d <- data.d[, var.d := (sd(duration) ^ 2)*((.N)-1)/(.N), keyby = .(gnrt)
	][, mean.d := mean(duration), keyby = .(gnrt, id.match)
	][, .(g.var.d = (sd(mean.d) ^ 2)*((.N)-1)/(.N), var.d = unique(var.d)), keyby = .(gnrt)
	][, within.d := (var.d - g.var.d) ^ (1 / 2)
	][, minus.d := (g.var.d) ^ (1 / 2) - within.d
	][, g.sd.d := g.var.d ^ (1 / 2)]


# point
data.p <- data[data.smlt, on = .(gnrt), nomatch = 0]
data.p <- yang[data.p, on = .(id)]
data.p <- data.p[, var.p := (sd(point) ^ 2)*((.N) - 1) / (.N), keyby = .(gnrt)
	][, mean.p := mean(point), keyby = .(gnrt, id.match)
	][, .(g.var.p = (sd(mean.p) ^ 2) * ((.N) - 1) / (.N), var.p = unique(var.p)), keyby = .(gnrt)
	][, within.p := (var.p - g.var.p) ^ (1 / 2)
	][, minus.p := (g.var.p) ^ (1 / 2) - within.p
	][, g.sd.p := g.var.p ^ (1 / 2)]

# income
data.i <- data[data.smlt, on = .(gnrt), nomatch = 0]
data.i <- yang[data.i, on = .(id)]
data.i <- data.i[, var.i := (sd(income) ^ 2) * ((.N) - 1) / (.N), keyby = .(gnrt)
	][, mean.i := mean(income), keyby = .(gnrt, id.match)
	][, .(g.var.i = (sd(mean.i) ^ 2) * ((.N) - 1) / (.N), var.i = unique(var.i)), keyby = .(gnrt)
	][, within.i := (var.i - g.var.i) ^ (1 / 2)
	][, minus.i := (g.var.i) ^ (1 / 2) - within.i
	][, g.sd.i := g.var.i ^ (1 / 2)]

data.total <- data.d[data.p, on = .(gnrt)]
data.total <- data.i[data.total, on = .(gnrt)]

fwrite(data.total, "C:/Users/shenfan/Desktop/yang/16.1/lyy.csv")


within.d <- ggplot(data.total, aes(x = within.d)) +
	geom_density(color = "black", fill = "gray") +
	geom_vline(xintercept = 0.6445) +
	labs(x = '平均搜寻期数', y = "核密度") +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank())

ggsave('C:/Users/shenfan/Desktop/yang/16.1/withind.jpg', within.d, width = 5, height = 4)

group.d <- ggplot(data.total, aes(x = g.sd.d)) +
	geom_density(color = "black", fill = "gray") +
	geom_vline(xintercept = 0.580238956) +
	labs(x = '平均搜寻期数', y = "核密度") +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank())

ggsave('C:/Users/shenfan/Desktop/yang/16.1/groupd.jpg', group.d, width = 5, height = 4)

minus.d <- ggplot(data.total, aes(x = minus.d)) +
	geom_density(color = "black", fill = "gray") +
	geom_vline(xintercept = -0.0643) +
	labs(x = '平均搜寻期数', y = "核密度") +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank())

ggsave('C:/Users/shenfan/Desktop/yang/16.1/minusd.jpg', minus.d, width = 5, height = 4)

within.p <- ggplot(data.total, aes(x = within.p)) +
	geom_density(color = "black", fill = "gray") +
	geom_vline(xintercept = 31.2050) +
	labs(x = '平均接受点数', y = "核密度") +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank())

ggsave('C:/Users/shenfan/Desktop/yang/16.1/withinp.jpg', within.p, width = 5, height = 4)

group.p <- ggplot(data.total, aes(x = g.sd.p)) +
	geom_density(color = "black", fill = "gray") +
	geom_vline(xintercept = 25.54007913) +
	labs(x = '平均接受点数', y = "核密度") +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank())

ggsave('C:/Users/shenfan/Desktop/yang/16.1/groupp.jpg', group.p, width = 5, height = 4)

minus.p <- ggplot(data.total, aes(x = minus.p)) +
	geom_density(color = "black", fill = "gray") +
	geom_vline(xintercept = -5.6649) +
	labs(x = '平均接受点数', y = "核密度") +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank())

ggsave('C:/Users/shenfan/Desktop/yang/16.1/minusp.jpg', minus.p, width = 5, height = 4)

within.i <- ggplot(data.total, aes(x = within.i)) +
	geom_density(color = "black", fill = "gray") +
	geom_vline(xintercept = 19.0471) +
	labs(x = '平均搜寻收益', y = "核密度") +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank())

ggsave('C:/Users/shenfan/Desktop/yang/16.1/withini.jpg', within.i, width = 5, height = 4)

group.i <- ggplot(data.total, aes(x = g.sd.i)) +
	geom_density(color = "black", fill = "gray") +
	geom_vline(xintercept = 16.36374964) +
	labs(x = '平均搜寻收益', y = "核密度") +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank())

ggsave('C:/Users/shenfan/Desktop/yang/16.1/groupi.jpg', group.i, width = 5, height = 4)

minus.i <- ggplot(data.total, aes(x = minus.i)) +
	geom_density(color = "black", fill = "gray") +
	geom_vline(xintercept = -2.6833) +
	labs(x = '平均搜寻收益', y = "核密度") +
	theme(
		panel.border = element_blank(),
		panel.background = element_blank())

ggsave('C:/Users/shenfan/Desktop/yang/16.1/minusi.jpg', minus.i, width = 5, height = 4)






