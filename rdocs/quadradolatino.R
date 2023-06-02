pacman::p_load(tidyverse)

# 1.1) Modelo: ----

# y_{ijk} = \mu + \alpha_i + \tau_j + \Beta_k + \epsilon_{ijk}

# 1.2) ANOVA: ----

dados <- c(432,518,458,583,331,
           724,478,524,550,400,
           489,384,556,297,420,
           494,500,313,486,501,
           515,660,438,394,318)

col <- c("Aj","Bj","Cj","Dj","Ej")

row <- c("Ai","Bi","Ci","Di","Ei")

trat <- c("D","A","B","C","E",
          "C","E","A","B","D",
          "E","B","C","D","A",
          "B","D","E","A","C",
          "A","C","D","E","B")

col <- rep(col,5)
row <- rep(row,each=5)

df <- data.frame(dados,row,col,trat)

anova <- aov(dados ~ trat+col+row)
summary(anova)

# 1.3) Pressupostos?

# Independência:

#plot(anova$residuals)

# resíduos pelos valores ajustados
plot(anova$residuals,anova$fitted.values)


# Normalidade:
qqnorm(anova$residuals)
qqline(anova$residuals)

shapiro.test(anova$residuals)

# Heterocedasticidade

#pacman::p_load(car)
#leveneTest(anova$residuals ~ trat)

bartlett.test(dados ~ trat)
boxplot(dados ~ trat)

# Checar a homogeneidade em linhas e colunas também

bartlett.test(dados ~ col)
bartlett.test(dados ~ row)

# Aditividade

lm(dados ~ trat+col+row)
lm(dados ~ trat+col)
lm(dados ~ trat+row)
lm(dados ~ col+row)
lm(dados ~ row)
lm(dados ~ col)
lm(dados ~ trat)

pacman::p_load(asbio)

# Verificando 2 a 2

tukey.add.test(dados,trat,col)
tukey.add.test(dados,trat,row)
tukey.add.test(dados,col,row)

# Modelo inteiro:
aditividade <- lm(dados ~ trat+col+row)

v_adt <- (predict(aditividade))^2

modelo <- lm(dados ~ trat+col+row+v_adt)

# Verificando
anova2 <- anova(aditividade,modelo)

# tá dando pau

# 1.4) Estimativa dos parâmetros ----

# Parâmetros: \mu | \alpha_i | \tau_j | \Beta_k

# \mu = 

# \alpha_i = 

# \tau_j = 

# \Beta_k = 

# 1.5) Variação total explicada pelo modelo ----

SQE <- summary(anova)[[1]][4,2]
SQT <- sum(summary(anova)[[1]][,2])

1-(SQE/SQT) # Total da variação explicada pelo modelo: 86,76%

# 1.6) Existe diferença? Tukey. ----

TukeyHSD(anova)

# --------------------------------------------------------------------------- #

dados <- c(19.56,23.16,29.72,
           22.94,27.51,23.71,
           25.06,17.70,22.32,
           23.24,23.54,18.75,
           16.28,22.29,28.09,
           18.53,19.89,20.42,
           23.98,20.46,19.28,
           15.33,23.02,24.97,
           24.41,22.44,19.23,
           16.65,22.69,24.94,
           18.96,24.19,21.95,
           21.49,15.78,24.65)

trat <- c("A","B","C",
          "B","C","A",
          "C","A","B",
          "B","C","A",
          "A","B","C",
          "C","A","B",
          "C","A","B",
          "A","B","C",
          "B","C","A",
          "A","B","C",
          "B","C","A",
          "C","A","B")

col <- c("C1","C2","C3")

row <- c("L1","L2","L3","L4","L5","L6",
         "L7","L8","L9","L10","L11","L12")

col <- rep(col,12)
row <- rep(row,each=3)

df <- data.frame(dados,row,col,trat)

anova <- aov(dados ~ trat+col+row)
summary(anova)

# 1.3) Pressupostos?

# Independência:

#plot(anova$residuals)

# resíduos pelos valores ajustados
plot(anova$residuals,anova$fitted.values)


# Normalidade:
qqnorm(anova$residuals)
qqline(anova$residuals)

shapiro.test(anova$residuals)

# Heterocedasticidade

#pacman::p_load(car)
#leveneTest(anova$residuals ~ trat)

bartlett.test(dados ~ trat)
boxplot(dados ~ trat)

# Checar a homogeneidade em linhas e colunas também

bartlett.test(dados ~ col)
bartlett.test(dados ~ row)

# Aditividade

#lm(dados ~ trat+col+row)
#lm(dados ~ trat+col)
#lm(dados ~ trat+row)
#lm(dados ~ col+row)
#lm(dados ~ row)
#lm(dados ~ col)
#lm(dados ~ trat)

pacman::p_load(asbio)

# Verificando 2 a 2

tukey.add.test(dados,trat,col)
tukey.add.test(dados,trat,row)
tukey.add.test(dados,col,row)

# Modelo inteiro:
aditividade <- lm(dados ~ trat+col+row)

v_adt <- (predict(aditividade))^2

modelo <- lm(dados ~ trat+col+row+v_adt)

# Verificando
anova2 <- anova(aditividade,modelo)
anova2

# 1.4) Estimativa dos parâmetros ----

# Parâmetros: \mu | \alpha_i | \tau_j | \Beta_k

# \mu = 

# \alpha_i = 

# \tau_j = 

# \Beta_k = 

# 1.5) Variação total explicada pelo modelo ----

SQE <- summary(anova)[[1]][4,2]
SQT <- sum(summary(anova)[[1]][,2])

1-(SQE/SQT) # Total da variação explicada pelo modelo: 86,76%

# 1.6) Existe diferença? Tukey. ----

TukeyHSD(anova)
