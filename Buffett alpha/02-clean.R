load("SJ2.RData")

# 加入行业
industry<-read_excel("C:/Users/shenfan/Desktop/Buffett alpha/行业.xlsx")
industry <- as.data.table(industry)
setnames(industry, 1:3, c("code", "name", "industry"))
industry <- industry[, stock.id := substring(code, 1, 6)
	][, c("name", "code") := NULL
	][!industry == "金融"]

variable <- industry[variable, on = .(stock.id), nomatch = 0]

##标准化Z：Z=(r-μ）/ σ μ为横截面均值 ，σ为r的横截面标准差。
############################################################################
# beta和IVOL得分越低越好,降序
# win
data <- variable
a1 <- quantile(data[, beta], 0.99, na.rm = TRUE)
a2 <- quantile(data[, beta], 0.01, na.rm = TRUE)
data <- data[order(beta)
	][, extremum := ifelse(beta > a1, 2, ifelse(beta < a2, 1, 0))
	][extremum == 1, beta := beta[.N]
	][extremum == 2, beta := beta[1]]

a1 <- quantile(data[, IVOL], 0.99, na.rm = TRUE)
a2 <- quantile(data[, IVOL], 0.01, na.rm = TRUE)
data <- data[order(IVOL)
	][, extremum := ifelse(IVOL > a1, 2, ifelse(IVOL < a2, 1, 0))
	][extremum == 1, IVOL := IVOL[.N]
	][extremum == 2, IVOL := IVOL[1]]

# beta
beta <- data
beta <- beta[!is.na(beta)
	][order(year2, month, - beta)
	][, beta.c := seq(1:(.N)), keyby = .(year2, month)
	][, beta.z := (beta.c - mean(beta.c)) / sd(beta.c), keyby = .(year2, month)
	][, .(stock.id, year2, month, beta.z)]

# IVOL
IVOL <- data
IVOL <- IVOL[!is.na(IVOL)
	][order(year2, month, - IVOL)
	][, IVOL.c := seq(1:(.N)), keyby = .(year2, month)
	][, IVOL.z := (IVOL.c - mean(IVOL.c, na.rm = TRUE)) / sd(IVOL.c, na.rm = TRUE), keyby = .(year2, month)
	][, .(stock.id, year2, month, IVOL.z)]

# safety
safety <- data[, .(stock.id, year2, month)]
safety <- beta[safety, on = .(stock.id, year2, month)]
safety <- IVOL[safety, on = .(stock.id, year2, month)]
safety <- safety[, safety := beta.z + IVOL.z
	][order(year2, month, safety)
	][!is.na(safety)
	][, safety.c := seq(1:(.N)), keyby = .(year2, month)
	][, safety.z := (safety.c - mean(safety.c, na.rm = TRUE)) / sd(safety.c, na.rm = TRUE), keyby = .(year2, month)
	][, .(stock.id, year2, month, safety.z)]

############################################################################
# cheapness
a1 <- quantile(data[, BM], 0.99, na.rm = TRUE)
a2 <- quantile(data[, BM], 0.01, na.rm = TRUE)
data <- data[order(BM)
	][, extremum := ifelse(BM > a1, 2, ifelse(BM < a2, 1, 0))
	][extremum == 1, BM := BM[.N]
	][extremum == 2, BM := BM[1]]

a1 <- quantile(data[, ADR], 0.99, na.rm = TRUE)
a2 <- quantile(data[, ADR], 0.01, na.rm = TRUE)
data <- data[order(ADR)
	][, extremum := ifelse(ADR > a1, 2, ifelse(ADR < a2, 1, 0))
	][extremum == 1, ADR := ADR[.N]
	][extremum == 2, ADR := ADR[1]]

a1 <- quantile(data[, RD], 0.99, na.rm = TRUE)
a2 <- quantile(data[, RD], 0.01, na.rm = TRUE)
data <- data[order(RD)
	][, extremum := ifelse(RD > a1, 2, ifelse(RD < a2, 1, 0))
	][extremum == 1, RD := RD[.N]
	][extremum == 2, RD := RD[1]]

# BM
BM <- data
BM <- BM[!is.na(BM)
	][order(year2, month, BM)
	][, BM.c := seq(1:(.N)), keyby = .(year2, month)
	][, BM.z := (BM.c - mean(BM.c)) / sd(BM.c), keyby = .(year2, month)
	][, .(stock.id, year2, month, BM.z)]

# ADR
ADR <- data
ADR <- ADR[!is.na(ADR)
	][order(year2, month, ADR)
	][, ADR.c := seq(1:(.N)), keyby = .(year2, month)
	][, ADR.z := (ADR.c - mean(ADR.c)) / sd(ADR.c), keyby = .(year2, month)
	][, .(stock.id, year2, month, ADR.z)]

# RD
RD <- data
RD <- RD[!is.na(RD)
	][order(year2, month, RD)
	][, RD.c := seq(1:(.N)), keyby = .(year2, month)
	][, RD.z := (RD.c - mean(RD.c)) / sd(RD.c), keyby = .(year2, month)
	][, .(stock.id, year2, month, RD.z)]

# cheapness
cheapness <- data[, .(stock.id, year2, month)]
cheapness <- BM[cheapness, on = .(stock.id, year2, month)]
cheapness <- ADR[cheapness, on = .(stock.id, year2, month)]
cheapness <- RD[cheapness, on = .(stock.id, year2, month)]
cheapness <- cheapness[, cheapness := BM.z + ADR.z + RD.z
	][order(year2, month, cheapness)
	][!is.na(cheapness)
	][, cheapness.c := seq(1:(.N)), keyby = .(year2, month)
	][, cheapness.z := (cheapness.c - mean(cheapness.c, na.rm = TRUE)) / sd(cheapness.c, na.rm = TRUE), keyby = .(year2, month)
	][, .(stock.id, year2, month, cheapness.z)]

##############################################################################
# Quality
a1 <- quantile(data[, GPOA], 0.99, na.rm = TRUE)
a2 <- quantile(data[, GPOA], 0.01, na.rm = TRUE)
data <- data[order(GPOA)
	][, extremum := ifelse(GPOA > a1, 2, ifelse(GPOA < a2, 1, 0))
	][extremum == 1, GPOA := GPOA[.N]
	][extremum == 2, GPOA := GPOA[1]]

a1 <- quantile(data[, ACC], 0.99, na.rm = TRUE)
a2 <- quantile(data[, ACC], 0.01, na.rm = TRUE)
data <- data[order(ACC)
	][, extremum := ifelse(ACC > a1, 2, ifelse(ACC < a2, 1, 0))
	][extremum == 1, ACC := ACC[.N]
	][extremum == 2, ACC := ACC[1]]

a1 <- quantile(data[, NOA], 0.99, na.rm = TRUE)
a2 <- quantile(data[, NOA], 0.01, na.rm = TRUE)
data <- data[order(NOA)
	][, extremum := ifelse(NOA > a1, 2, ifelse(NOA < a2, 1, 0))
	][extremum == 1, NOA := NOA[.N]
	][extremum == 2, NOA := NOA[1]]

# GPOA
GPOA <- data
GPOA <- GPOA[!is.na(GPOA)
	][order(year2, month, GPOA)
	][, GPOA.c := seq(1:(.N)), keyby = .(year2, month)
	][, GPOA.z := (GPOA.c - mean(GPOA.c)) / sd(GPOA.c), keyby = .(year2, month)
	][, .(stock.id, year2, month, GPOA.z)]

# ACC
ACC <- data
ACC <- ACC[!is.na(ACC)
	][order(year2, month, - ACC)
	][, ACC.c := seq(1:(.N)), keyby = .(year2, month)
	][, ACC.z := (ACC.c - mean(ACC.c)) / sd(ACC.c), keyby = .(year2, month)
	][, .(stock.id, year2, month, ACC.z)]

# NOA
NOA <- data
NOA <- NOA[!is.na(NOA)
	][order(year2, month, - NOA)
	][, NOA.c := seq(1:(.N)), keyby = .(year2, month)
	][, NOA.z := (NOA.c - mean(NOA.c)) / sd(NOA.c), keyby = .(year2, month)
	][, .(stock.id, year2, month, NOA.z)]

# quality
quality <- data[, .(stock.id, year2, month)]
quality <- GPOA[quality, on = .(stock.id, year2, month)]
quality <- ACC[quality, on = .(stock.id, year2, month)]
quality <- NOA[quality, on = .(stock.id, year2, month)]
quality <- quality[, quality := GPOA.z + ACC.z + NOA.z
	][order(year2, month, quality)
	][!is.na(quality)
	][, quality.c := seq(1:(.N)), keyby = .(year2, month)
	][, quality.z := (quality.c - mean(quality.c, na.rm = TRUE)) / sd(quality.c, na.rm = TRUE), keyby = .(year2, month)
	][, .(stock.id, year2, month, quality.z)]

##############################################################################
Bscore <- data[, .(stock.id, year2, month)]
Bscore <- safety[Bscore, on = .(stock.id, year2, month)]
Bscore <- cheapness[Bscore, on = .(stock.id, year2, month)]
Bscore <- quality[Bscore, on = .(stock.id, year2, month)]

Bscore <- Bscore[, Bscore := safety.z + cheapness.z + quality.z
	][order(year2, month, Bscore)
	][!is.na(Bscore)
	][, Bscore.c := seq(1:(.N)), keyby = .(year2, month)
	][, Bscore.z := (Bscore.c - mean(Bscore.c, na.rm = TRUE)) / sd(Bscore.c, na.rm = TRUE), keyby = .(year2, month)
	][, .(stock.id, year2, month, Bscore.z)]

save(Bscore, file = "SJBscore.RData")


