pacman::p_load(car,asbio,easyanova,tidyverse)

dados <- c(18.2,18.9,12.9,14.4,
           27.2,24,22.4,22.5,
           15.7,14.5,15.1,14.2,
           41,43.9,36.3,39.9)
fata <- factor(rep(c(-1,1,-1,1),each=4))
fatb <- factor(rep(c(-1,-1,1,1),each=4))
bloco <- factor(rep(c("I","II","III","IV"),4))            
df <- data.frame(dados,fata,fatb,bloco)

anova <- aov(dados~fata*fatb+bloco)
summary(anova)

# Normalidade
shapiro.test(anova$residuals)

# Homocedasticidade
leveneTest(dados~fata)
leveneTest(dados~fatb)
leveneTest(dados~bloco)
leveneTest(dados~fata:fatb)

bartlett.test(dados~fata)
bartlett.test(dados~fatb)
bartlett.test(dados~bloco)
bartlett.test(dados~fata:fatb) # ?

# Independência
plot(dados)
plot(anova$residuals)
plot(anova$residuals~anova$fitted.values)

TukeyHSD(anova)
boxplot(dados~fata)
boxplot(dados~fatb)
boxplot(dados~fata:fatb)

# 2

dados <- c(107,117,122,111,90,116,
           89,101,98,101,95,90,
           116,136,130,122,117,114,
           101,110,104,91,100,94,
           90,112,99,105,110,114,
           96,89,92,78,90,93)
fata <- factor(rep(c("A","B","C"),each=12))
fatb <- factor(rep(rep(c("M","P"),each=6),3))
bloco <- factor(rep(c("I","II","III","IV","V","VI"),6))
df <- data.frame(dados,fata,fatb,bloco)

anova <- aov(dados~fata*fatb+bloco)
summary(anova)

anova2 <- aov(dados~fata*fatb+Error(bloco/fata))
summary(anova2)

shapiro.test(anova$residuals)

leveneTest(dados~fata)
leveneTest(dados~fatb)
leveneTest(dados~bloco)
leveneTest(dados~fata:fatb)

plot(anova$residuals)


aditividade <- lm(dados~fata*fatb+bloco)
v_adt <- (predict(aditividade)^2)
modelo <- lm(dados~fata*fatb+bloco+v_adt)
ANOVA <- aov(aditividade,modelo)
summary(ANOVA)

TukeyHSD(anova)

boxplot(dados~fata)
boxplot(dados~fatb)
boxplot(dados~fata:fatb)
