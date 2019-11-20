load("datamain5.RData")

data<-data.main5

liner1 <- plm(r.total.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner2 <- plm(r.capm.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner3 <- plm(r.fama.c ~ netflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner4 <- plm(r.total.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner5 <- plm(r.capm.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner6 <- plm(r.fama.c ~ inflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

liner7 <- plm(r.total.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner8 <- plm(r.capm.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))
liner9 <- plm(r.fama.c ~ outflow.1 + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ"))

stargazer(liner1, liner2, liner3, liner4, liner5, liner6, liner7, liner8, liner9, type = "html", out = "C://Users//shenfan//Desktop//data//lm//再一版本吧//1(1).doc", add.lines = list(c("fund", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes"), c("time", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")))

data <- data.main5
#风格的不同有一定的不同
data <- data[, st.mixed := ifelse(style == "mixed", 1, 0)
	][, st.value := ifelse(style == "value", 1, 0)
	][, st.growth := ifelse(style == "growth", 1, 0)]

summary(liner1 <- plm(netflow ~ netflow.1 * st.growth + netflow.1 * st.mixed + netflow.2 * st.growth + netflow.2 * st.mixed + netflow.3 * st.growth + netflow.3 * st.mixed + netflow.4 * st.growth + netflow.4 * st.mixed + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))

summary(liner1 <- plm(netflow ~ netflow.1  + netflow.2  + netflow.3  + netflow.4  + logfund_size.1 + logfund_age.1 + period_return.1, data, model = "within", effect = "twoways", index = c("id", "DateQ")))
