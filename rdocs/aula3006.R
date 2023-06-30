A <- c(-1,1,-1,1,-1,1,-1,1)
B <- c(-1,-1,1,1,-1,-1,1,1)
C <- c(-1,-1,-1,-1,1,1,1,1)
dados <- c(550,604,
           669,650,
           633,601,
           642,635,
           1037,1052,
           749,868,
           1075,1063,
           729,860)

rep1 <- c(550,
          669,
          633,
          642,
          1037,
          749,
          1075,
          729)

rep2 <- c(604,
          650,
          601,
          635,
          1052,
          868,
          1063,
          860)

df <- data.frame(A,B,C,rep1,rep2)
pacman::p_load(tidyverse)
df <- df %>%
  mutate(total = rep1 + rep2,
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
r <- 2
## N
n <- r*2^k


sinais <- df %>%
  select(!c(rep1,rep2,total))

contraste <- t(sinais) %*% df$total

efeito <- contraste/(r * 2^(k-1))

#with(df, interaction.plot(A, B, total))
#with(df, interaction.plot(A, C, total))
#with(df, interaction.plot(B, C, total))

SQ <- contraste^2/(r * 2^k)

SQTot <- sum(df$total^2) - (sum(df$total)^2/n)

SQRes <- SQTot - sum(SQ)

anova <- aov(df$total ~ A*B*C)
summary(anova)

pacman::p_load(dae)
interaction.ABC.plot(total, A, B, C, data = df)

