# ---- 1)

dados <- c(7,5,17,8,4,9,5,4,10,9,6,12,10,3,13)
tratamento <- factor(rep(1:3,5))
df <- data.frame(dados,tratamento)

pacman::p_load(tidyverse)

anova <- aov(dados ~ tratamento)
summary(anova)

boxplot(dados ~ tratamento)

shapiro.test(anova[[2]])

plot(anova[[2]])

bartlett.test(anova[[2]] ~ tratamento)

tapply(dados,tratamento,mean)

TukeyHSD(anova)

# ---- 2)

rm(list = ls())

dados <- c(105.17,102.21,99.43,107.74,106.2,
           88.42,89.36,90.16,92.3,91.5,
           100.78,99.26,96.77,102.5,104.1,
           102.09,99.45,102.63,107.63,105.9
           )
tratamento <- factor(rep(1:4,each=5))
bloco <- factor(rep(c("NO","NE","CO","SE","SU"),4))
df <- data.frame(dados,tratamento,bloco)

anova <- aov(dados ~ tratamento + bloco)
summary(anova)

shapiro.test(anova[[2]])

plot(anova[[2]])

bartlett.test(anova[[2]] ~ tratamento)

TukeyHSD(anova)

# pacman::p_load(asbio)
# tukey.add.test(dados,tratamento,bloco)


taui <- c(0,-2,0,2)
b <- length(unique(bloco))
a <- length(unique(tratamento))
sigma2 <- 2.72 # QMRes
alpha <- .05


fcrit <- (2.72)/(204.95+23.92+2.72)
b <- 5
delta <- b*sum(taui^2/sigma2)
qf(fcrit,df1=a-1,df2=(a-1)*(b-1),ncp=delta)

