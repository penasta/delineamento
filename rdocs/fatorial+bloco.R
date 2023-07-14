# --------------------------------------------------------------------------- #

fata <- rep(c("A1","A2","A3","A4"),each=9)
fatb <- rep(c("B1","B2","B3"),12)
bloco <- rep(rep(c("bloco1","bloco2","bloco3"),each=4),3)
dados <- c(30,34,29,28,31,31,31,35,32,
           35,41,26,32,36,30,37,40,34,
           37,38,33,40,42,32,41,39,39,
           36,42,36,41,40,40,40,44,45)

df <- data.frame(dados,bloco,fata,fatb)

# anova <- aov(dados ~ fata * fatb + bloco)
# summary(anova)

# --------------------------------------------------------------------------- #

dados <- c(4.2,4.6,4.5,4.4,
           4.5,4.7,4.3,4.7,
           5.2,5,6.8,5.8,
           3.8,4.4,4.8,3.9,
           3.7,3.5,3.1,3.7,
           3.5,3.1,3.4,3.3,
           4.2,4.2,5.2,5.1,
           4,3.8,3.7,4.1,
           3.9,3.9,3.7,4)

fata <- rep(c("aração","aração+gradagem","subsolagem"),each=12)
fatb <- rep(rep(c("A","B","C"),each=4),3)
bloco <- rep(c("I","II","III","IV"),9)

df <- data.frame(dados,fata,fatb,bloco)

anova <- aov(dados ~ fata*fatb + Error(bloco/fata))
summary(anova)

anova2 <- aov(dados ~ fata*fatb + bloco:fata)
summary(anova2)

# Normalidade
qqnorm(anova2$residuals)
qqline(anova2$residuals)
shapiro.test(anova2$residuals)

# Heterocedasticidade
bartlett.test(anova2$residuals ~ fata)
bartlett.test(anova2$residuals ~ fatb)
bartlett.test(anova2$residuals ~ bloco)
bartlett.test(anova2$residuals ~ c(fatb,fatb))


pacman::p_load(car)
leveneTest(anova2$residuals ~ fata)
leveneTest(anova2$residuals ~ fatb)
leveneTest(anova2$residuals ~ c(fatb,fatb))
leveneTest(anova2$residuals ~ bloco)

# Independencia
plot(anova2$residuals ~ anova2$fitted.values)
plot(anova2$residuals)

interaction.plot(fata,fatb,dados)

# Faz tudo:
pacman::p_load(easyanova)
ea2(data.frame(fata,bloco,fatb,dados),design=5)


# --------------------------------------------------------------------------- #