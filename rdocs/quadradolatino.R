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
#anova2 <- aov(aditividade,modelo)

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

