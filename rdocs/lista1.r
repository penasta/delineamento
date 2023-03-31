# --------------------------------------------------------------------------- #
#                                   Lista 1                                   #
# --------------------------------------------------------------------------- #

# 1) ----
x <- 85
mu <- 100
dp <- 12
n <- 16

xcrit  <- (qt(.1,(n-1))*sqrt(dp^2/n)+mu)
xcrit

tcrit <- qt(.1,n-1)

tcalc <- (x-mu)/(dp/sqrt(n))

#?pt
pt(tcalc,n-1)

# --------------------------------------------------------------------------- #

# 2) ----

A <- c(13.6,13.6,14.7,12.1,12.3,13.2,11,12.4)
D <- c(11.4,12.5,14.6,13,11.7,10.3,9.8,10.4)

library(pacman)
#p_load(nortest)

shapiro.test(A)
shapiro.test(D)

dif <- A-D
mean(dif)
sd(dif)

shapiro.test(dif)

tobs <- mean(dif)/(sd(dif)/sqrt(8))
pt(tobs,8-1,lower.tail = F)
# Rejeita

# direto:
t.test(dif)

t.test(A,D,paired=T,alternative = "greater")



# --------------------------------------------------------------------------- #

# 3) ----

VA <- c(1.3,1.4,1.1,1.4,1.5)
VB <- c(1.8,1.6,1.9,1.9,1.8)

shapiro.test(VA)
shapiro.test(VB)

shapiro.test(VA-VB)

var.test(VA,VB)

t.test(VA,VB,var.equal=T)

# --------------------------------------------------------------------------- #
