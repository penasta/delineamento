options(scipen = 999)
pacman::p_load(tidyverse,dae)

# --------------------------------------------------------------------------- #

rep1 <- c(22,32,35,55,44,40,60,39)
rep2 <- c(31,43,34,47,45,37,50,41)
rep3 <- c(25,29,50,46,38,36,54,47)

A <- c(-1,1,-1,1,-1,1,-1,1)
B <- c(-1,-1,1,1,-1,-1,1,1)
C <- c(-1,-1,-1,-1,1,1,1,1)

df <- data.frame(rep1,rep2,rep3,A,B,C)

df <- df %>%
  mutate(total = rep1 + rep2 + rep3,
         A = A,
         B = B,
         C = C,
         AB = A*B,
         AC = A*C,
         BC = B*C,
         ABC = A*B*C,
         I = 1)

## Número de fatores
k <- 3
## Número de níveis
a <- b <- c <- 2
## Número de repetições
r <- 3
## N
n <- r*2^k

sinais <- df %>%
  select(!c(rep1,rep2,rep3,total))

contraste <- t(sinais[,-8]) %*% df$total

efeito <- contraste/(r * 2^(k-1))

with(df, interaction.plot(A, B, total))
with(df, interaction.plot(A, C, total))
with(df, interaction.plot(B, C, total))

SQ <- contraste^2/(r * 2^k)

SQTot <- sum(df$total^2) - (sum(df$total)^2/n)

SQRes <- SQTot - sum(SQ)

(glA <- a - 1)
(glB <- b - 1)
(glC <- c - 1)
(glAB <- (a-1)*(b-1))
(glAC <- (a-1)*(c-1))
(glBC <- (b-1)*(c-1))
(glABC <- (a-1)*(b-1)*(c-1))
(glRES <- a*b*c*(r-1))

(MQ <- SQ/c(glA, glB, glC, glAB, glAC, glBC, glABC))

# ---------------------------------------------------------------------------- #

# Arrumar isso daqui pra baixo

f <- MQ/MQRes
## Valores p
p <- pf(f, df1 = glA, df2 = glRES, lower.tail = FALSE)
## Tabela de ANOVA
tab.anova <- data.frame("GL" = c(glA, glB, glC, glAB, glAC, glBC, glABC, glRES),
                        "SQ" = c(SQ, SQRes),
                        "QM" = c(MQ, MQRes),
                        "F" = c(f, NA),
                        "p-valor" = c(p, NA),
                        row.names = c("T", "C", "K", "T:C", "T:K",
                                      "C:K", "T:C:K", "Resíduo"))



anova <- aov(df$total ~ A*B*C)
summary(anova)

interaction.ABC.plot(total, A, B, C, data = df)

fit <- lm(anova)
summary(fit)

fit <- lm(total ~ factor(A) * factor(B) * factor(C), data = df)
summary(fit)

m0 <- lm(total ~ A * B * C, data = df)
anova(m0)



# ---------------------------------------------------------------------------- #

dados <- c(22,31,25,32,43,29,35,34,50,55,47,46,44,45,38,40,37,36,60,50,54,39,41,47)
length(dados)

a <-2
b<-2
c<-2
n<-3
N<-a*b*c*n

fata <- c(-1,-1,-1,
          1,1,1,
          -1,-1,-1,
          1,1,1,)