setwd("C:/Users/soere/Documents/Studium/Düsseldorf/Bachelorarbeit")
load("~/Studium/Düsseldorf/Bachelorarbeit/Bewertung_Automarken.RData")

install.packages("gridExtra")
install.packages("ggplot2")
install.packages("expm")

library(gridExtra)
library(ggplot2)
library(expm)

######################################################################

###Deskriptive Statistik:

setwd("C:/Users/soere/Documents/Studium/Düsseldorf/Bachelorarbeit/Deskriptive Statistik")

names <- c("Wirtschaftlichkeit", "Service", "Werterhalt", "Preis", "Design", "Sportlichkeit", "Sicherheit", "Bedienung")
colnames(car) <- names

c1 <- car[,1]
c2 <- car[,2]
c3 <- car[,3]
c4 <- car[,4]
c5 <- car[,5]
c6 <- car[,6]
c7 <- car[,7]
c8 <- car[,8]


#Tabelle:
View(car)


#Summary:
summary(car) 

#emp. Standardabweichungen
sd_car <- numeric(8)
for(i in 1:8){
  sd_car[i] <- sd(car[,i])
  sd_car
}
round(sd_car, digits = 2)


#Boxplot (Abbildung 1):
png("Boxplots.png", width = 800, height = 600)
boxplot(car, col = "lightblue", xlab = "Variablen", ylab = "Noten")
points(1:8, colMeans(car), col = "red", pch = 19)
dev.off()


#QQ-Plot (Abbildung 2):
for(i in 1:8){
  png(paste('QQ-Plot_',names[i],'.png'), width = 400, height = 400)
  qqnorm(car[,i], main = paste('QQ-Plot ',names[i]), xlab = "Theoretische Quantile", ylab = "Empirische Quantile", pch = 16, cex = 1.5)
  qqline(car[,i], col = "red")
  dev.off()
}


#Histogramm (Abbildung 3):
for(i in 1:8){
  png(paste('Histogramm_',names[i],'.png'), width = 400, height = 400)
  hist(car[,i], breaks = 1:6, xlab = "Noten", ylab = "Häufigkeit", main = paste('Histogramm ', names[i]), col = c("darkgreen", "green", "yellow", "orange", "red"), freq = FALSE)
  daten <- rnorm(1000, mean = mean(car[,i]), sd = sd(car[,i]))
  x_vals <- seq(min(daten), max(daten), length = 200)
  y_vals <- dnorm(x_vals, mean = mean(daten), sd = sd(daten))
  lines(x_vals, y_vals, col = "red", lwd = 2)
  dev.off()
  }

######################################################################

###Kanonische Korrelationsanalyse:

setwd("C:/Users/soere/Documents/Studium/Düsseldorf/Bachelorarbeit/Kanonische Korrelationsanalyse R")

#2 Variablenmengen:
X <- cbind(c4, c3)                   ##d.h. p = 2
Y <- cbind(c1, c2, c5, c6, c7, c8)   ##d.h. q = 6


#Globaler F-Test:
lmc4 <- lm(c4 ~ Y)
summary(lmc4)

lmc3 <- lm(c3 ~ Y)
summary(lmc3)


#Streudiagramme (Abbildungen 4 & 5):
png("Werterhalt_vs1.png", width = 1700, height = 500)
par(mfrow=c(1, 3), mar = c(8, 5, 4, 2))
for(i in c(1:2, 5)){
  plot(car$Werterhalt, car[,i], main = paste('Werterhalt vs.', names[i]), xlab = "Werterhalt", ylab = names[i], cex.main = 2.5, cex.lab = 2)
  abline(lm(car$Werterhalt ~ car[,i]), col = "red", lwd = 2)
  mtext(paste('r =', round(cor(car$Werterhalt, car[,i]), 3)), side = 1, line = 6, cex = 2.5)
}
dev.off()

png("Werterhalt_vs2.png", width = 1700, height = 500)
par(mfrow=c(1, 3), mar = c(8, 5, 4, 2))
for(i in 6:8){
  plot(car$Werterhalt, car[,i], main = paste('Werterhalt vs.', names[i]), xlab = "Werterhalt", ylab = names[i], cex.main = 2.5, cex.lab = 2)
  abline(lm(car$Werterhalt ~ car[,i]), col = "red", lwd = 2)
  mtext(paste('r =', round(cor(car$Werterhalt, car[,i]), 3)), side = 1, line = 6, cex = 2.5)
}
dev.off()

png("Preis_vs1.png", width = 1700, height = 500)
par(mfrow=c(1, 3), mar = c(8, 5, 4, 2))
for(i in c(1:2, 5)){
  plot(car$Preis, car[,i], main = paste('Preis vs.', names[i]), xlab = "Preis", ylab = names[i], cex.main = 2.5, cex.lab = 2)
  abline(lm(car$Preis ~ car[,i]), col = "red", lwd = 2)
  mtext(paste('r =', round(cor(car$Preis, car[,i]), 3)), side = 1, line = 6, cex = 2.5)
}
dev.off()

png("Preis_vs2.png", width = 1700, height = 500)
par(mfrow=c(1, 3), mar = c(8, 5, 4, 2))
for(i in 6:8){
  plot(car$Preis, car[,i], main = paste('Preis vs.', names[i]), xlab = "Preis", ylab = names[i], cex.main = 2.5, cex.lab = 2)
  abline(lm(car$Preis ~ car[,i]), col = "red", lwd = 2)
  mtext(paste('r =', round(cor(car$Preis, car[,i]), 3)), side = 1, line = 6, cex = 2.5)
}
dev.off()


#Stichprobenmittel:
Xmean <- colMeans(X)
Xmean

Ymean <- colMeans(Y)
Ymean


#Kovarianzmatrizen:
S11 <- cov(X) #S11 = (1 / (23 - 1)) * (t(X) - Xmean) %*% t(t(X) -Xmean)
S22 <- cov(Y)
S12 <- cov(X, Y)
S21 <- cov(Y, X)

round(S11, digits = 2)
round(S22, digits = 2)
round(S12, digits = 2)
round(S21, digits = 2)

S <- cov(cbind(X, Y)) #S = rbind(cbind(S11, S12), cbind(S21, S22))
round(S, digits = 2)


#Eigenwerte/-vektoren:
S_1 <- solve(sqrtm(S11)) %*% S12 %*% solve(S22) %*% S21 %*% solve(sqrtm(S11))

EW <- eigen(S_1)$values   
e <- eigen(S_1)$vectors * (-1)  #mit (-1) multiplizieren damit die Korrelation der kanonischen Variablen positiv ist.
                                #hat keinen Einfluss auf die Analyse, da Vektor immernoch normiert ist. Wenn man nicht mit (-1) multipliziert, müsste man allerdings bei der Interpretation aufpassen, da Cor(U,V) negativ wäre.


S_2 <- solve(sqrtm(S22)) %*% S21 %*% solve(S11) %*% S12 %*% solve(sqrtm(S22))

f <- eigen(S_2)$vectors #nur die ersten 2 Eigenvektoren sind für die kanonischen Variablen von Bedeutung
#alternativ für die ersten beiden Eigenvektoren (gleiches Ergebnis):
#f1 <- 1/sqrt(EW[1]) * solve(sqrtm(S22)) %*% S21 %*% solve(sqrtm(S11)) %*% e[,1]
#f2 <- 1/sqrt(EW[2]) * solve(sqrtm(S22)) %*% S21 %*% solve(sqrtm(S11)) %*% e[,2]


#Kanonische Korrelationen & Koeffizientenvektoren der kanonischen Variablen:
p <- sqrt(EW) 
p1 <- p[1]
p2 <- p[2]


A <- t(solve(sqrtm(S11)) %*% e) #Koeffizientenvektoren in den Zeilen
a1 <- A[1,]
a2 <- A[2,]

B <- t(solve(sqrtm(S22)) %*% f)
b1 <- B[1,]
b2 <- B[2,]


#kanonische Variablen gegeneinander plotten (Abbildung 6):

#(U_1, V_1):
U_1 <- as.vector(a1 %*% t(X))
V_1 <- as.vector(b1 %*% t(Y))

png("1._Kanonische_Variablen.png", width = 400, height = 400)
plot(V_1, U_1, main = "1. Kanonische Variablen", xlab = expression(V[1]), ylab = expression(U[1]), pch = 19)
abline(lm(U_1 ~ V_1), col = "red", lwd = 2)
dev.off()

#(U_2, V_2):
U_2 <- as.vector(a2 %*% t(X))
V_2 <- as.vector(b2 %*% t(Y))

png("2._Kanonische_Variablen.png", width = 400, height = 400)
plot(V_2, U_2, main = "2. Kanonische Variablen", xlab = expression(V[2]), ylab = expression(U[2]), pch = 19)
abline(lm(U_2 ~ V_2), col = "red", lwd = 2)
dev.off()


###################################

##Interpretation:
#Korrelation zwischen kan. Var. & zugehörigenden Variablen:
D1 <- diag(2)
for(i in 1:2){
  D1[i,i] <- var(X[,i])
}

D2 <- diag(6)
for(i in 1:6){
  D2[i,i] <- var(Y[,i])
}

R_UX <- A %*% S11 %*% solve(sqrtm(D1))
round(R_UX, digits = 2)

R_VY <- B %*% S22 %*% solve(sqrtm(D2))
round(R_VY[1:2,], digits = 2)

#Korrelation zwischen kan. Var. & anderen Variablen:
R_UY <- A %*% S12 %*% solve(sqrtm(D2))
round(R_UY, digits = 2)

R_VX <- B %*% S21 %*% solve(sqrtm(D1))
round(R_VX[1:2,], digits = 2)


#Fehlermatrizen:
F22 <- solve(B)[,-(1:2)] %*% t(solve(B)[,-(1:2)])
round(F22, digits = 2)


#Anteil der Totalvarianz von Y, die von den ersten kanonischen Variablen erklärt werden:

#Anteil der ersten beiden kanonischen Variable V_1 & V_2:
R <- 0
for(i in 1:6){
    R = R + ((t(R_VY)[i,1]^2 + t(R_VY)[i,2]^2) * var(Y[,i]))
}
R_V <- R / sum(diag(S22))
#alternativ: sum(diag(solve(B)[,1:2] %*% t(solve(B)[,1:2]))) / sum(diag(S22))
#alternativ: sum(diag(solve(B)[,1] %*% t(solve(B)[,1]) + solve(B)[,2] %*% t(solve(B)[,2]))) / sum(diag(S22))
#alternativ: sum(diag(sqrtm(D2) %*% (t(R_VY)[,1:2]) %*% R_VY[1:2,] %*% sqrtm(D2))) / sum(diag(S22))
#alternativ: sum(diag(D2 %*% t(R_VY)[,1:2] %*% R_VY[1:2,])) / sum(diag(S22))
#alles dasselbe Ergebnis
round(R_V, digits = 2)
