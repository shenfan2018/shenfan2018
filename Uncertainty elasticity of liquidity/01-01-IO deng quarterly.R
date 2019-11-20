# IO, social fund, mutual fund, QFII, insurance
time <- c("2004-03-31", "2004-06-30", "2004-09-30", "2004-12-31", "2005-03-31", "2005-06-30", "2005-09-30", "2005-12-31", "2006-03-31", "2006-06-30", "2006-09-30", "2006-12-31", "2007-03-31", "2007-06-30", "2007-09-30", "2007-12-31", "2008-03-31", "2008-06-30", "2008-09-30", "2008-12-31", "2009-03-31", "2009-06-30", "2009-09-30", "2009-12-31", "2010-03-31", "2010-06-30", "2010-09-30", "2010-12-31", "2011-03-31", "2011-06-30", "2011-09-30", "2011-12-31", "2012-03-31", "2012-06-30", "2012-09-30", "2012-12-31", "2013-03-31", "2013-06-30", "2013-09-30", "2013-12-31", "2014-03-31", "2014-06-30", "2014-09-30", "2014-12-31", "2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31", "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31")

# IO
IO <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/机构持股比例.xlsx')
IO <- as.data.table(IO)
setnames(IO, 1:58, c("code", "name", time))
IO = melt(IO, id.vars = c("code", "name"), measure.vars = time)
setnames(IO, c("value", "variable"), c("IO", "date"))

IO <- IO[!is.na(name)
	][, IO := ifelse(is.na(IO), 0, IO)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IO)]


# add social
IOS <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/社保基金持股比例.xlsx')
IOS <- as.data.table(IOS)
setnames(IOS, 1:58, c("code", "name", time))
IOS = melt(IOS, id.vars = c("code", "name"), measure.vars = time)
setnames(IOS, c("value", "variable"), c("IOS", "date"))

IOS <- IOS[!is.na(name)
	][, IOS := ifelse(is.na(IOS), 0, IOS)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOS)]

# match
IO <- IOS[IO, on = .(stock.id, date)]


# add mutual
IOF <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/基金持股比例.xlsx')
IOF <- as.data.table(IOF)
setnames(IOF, 1:58, c("code", "name", time))
IOF = melt(IOF, id.vars = c("code", "name"), measure.vars = time)
setnames(IOF, c("value", "variable"), c("IOF", "date"))

IOF <- IOF[!is.na(name)
	][, IOF := ifelse(is.na(IOF), 0, IOF)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOF)]
# match
IO <- IOF[IO, on = .(stock.id, date)]

# add QFII
IOQ <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/QFII持股比例.xlsx')
IOQ <- as.data.table(IOQ)
setnames(IOQ, 1:58, c("code", "name", time))
IOQ = melt(IOQ, id.vars = c("code", "name"), measure.vars = time)
setnames(IOQ, c("value", "variable"), c("IOQ", "date"))

IOQ <- IOQ[!is.na(name)
	][, IOQ := ifelse(is.na(IOQ), 0, IOQ)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOQ)]
# match
IO <- IOQ[IO, on = .(stock.id, date)]


# add 保险公司
IOI <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/保险公司持股比例.xlsx')
IOI <- as.data.table(IOI)
setnames(IOI, 1:58, c("code", "name", time))
IOI = melt(IOI, id.vars = c("code", "name"), measure.vars = time)
setnames(IOI, c("value", "variable"), c("IOI", "date"))

IOI <- IOI[!is.na(name)
	][, IOI := ifelse(is.na(IOI), 0, IOI)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOI)]
# match
IO <- IOI[IO, on = .(stock.id, date)]


# add 券商
IOSE <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/券商持股比例.xlsx')
IOSE <- as.data.table(IOSE)
setnames(IOSE, 1:58, c("code", "name", time))
IOSE = melt(IOSE, id.vars = c("code", "name"), measure.vars = time)
setnames(IOSE, c("value", "variable"), c("IOSE", "date"))

IOSE <- IOSE[!is.na(name)
	][, IOSE := ifelse(is.na(IOSE), 0, IOSE)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOSE)]
# match
IO <- IOSE[IO, on = .(stock.id, date)]


# add 信托
IOT <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/信托公司持股比例.xlsx')
IOT <- as.data.table(IOT)
setnames(IOT, 1:58, c("code", "name", time))
IOT = melt(IOT, id.vars = c("code", "name"), measure.vars = time)
setnames(IOT, c("value", "variable"), c("IOT", "date"))

IOT <- IOT[!is.na(name)
	][, IOT := ifelse(is.na(IOT), 0, IOT)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOT)]
# match
IO <- IOT[IO, on = .(stock.id, date)]

# add 券商理财产品
IOM <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/券商理财持股比例.xlsx')
IOM <- as.data.table(IOM)
setnames(IOM, 1:58, c("code", "name", time))
IOM = melt(IOM, id.vars = c("code", "name"), measure.vars = time)
setnames(IOM, c("value", "variable"), c("IOM", "date"))

IOM <- IOM[!is.na(name)
	][, IOM := ifelse(is.na(IOM), 0, IOM)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOM)]
# match
IO <- IOM[IO, on = .(stock.id, date)]


# add 银行持股
IOB <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/银行持股比例.xlsx')
IOB <- as.data.table(IOB)
setnames(IOB, 1:58, c("code", "name", time))
IOB = melt(IOB, id.vars = c("code", "name"), measure.vars = time)
setnames(IOB, c("value", "variable"), c("IOB", "date"))

IOB <- IOB[!is.na(name)
	][, IOB := ifelse(is.na(IOB), 0, IOB)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOB)]
# match
IO <- IOB[IO, on = .(stock.id, date)]

# add 企业年金
IOC <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/企业年金持股比例.xlsx')
IOC <- as.data.table(IOC)
setnames(IOC, 1:58, c("code", "name", time))
IOC = melt(IOC, id.vars = c("code", "name"), measure.vars = time)
setnames(IOC, c("value", "variable"), c("IOC", "date"))

IOC <- IOC[!is.na(name)
	][, IOC := ifelse(is.na(IOC), 0, IOC)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOC)]
# match
IO <- IOC[IO, on = .(stock.id, date)]


# add 阳光私募
IOP <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/阳光私募持股比例.xlsx')
IOP <- as.data.table(IOP)
setnames(IOP, 1:58, c("code", "name", time))
IOP = melt(IOP, id.vars = c("code", "name"), measure.vars = time)
setnames(IOP, c("value", "variable"), c("IOP", "date"))

IOP <- IOP[!is.na(name)
	][, IOP := ifelse(is.na(IOP), 0, IOP)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IOP)]
# match
IO <- IOP[IO, on = .(stock.id, date)]


# add 非金融类上市公司
IONF <- read_excel('C:/Users/shenfan/Desktop/Uncertainty elasticity of liquidity/data/非金融类上市公司持股比例.xlsx')
IONF <- as.data.table(IONF)
setnames(IONF, 1:58, c("code", "name", time))
IONF = melt(IONF, id.vars = c("code", "name"), measure.vars = time)
setnames(IONF, c("value", "variable"), c("IONF", "date"))

IONF <- IONF[!is.na(name)
	][, IONF := ifelse(is.na(IONF), 0, IONF)
	][, stock.id := substring(code, 1, 6)
	][, date := as.Date(date)
	][, .(stock.id, date, IONF)]
# match
IO <- IONF[IO, on = .(stock.id, date)]


IO <- IO[, sum := IOF + IOI + IOQ + IOS + IOSE + IOT + IOM + IOB + IOC + IOP + IONF
	][, dif := IO - sum]

save(IO, file = 'IOquarter.RData')
