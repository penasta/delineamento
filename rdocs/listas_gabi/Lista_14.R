n <- 3
valores <- c(22,31,25,
             32,43,29,
             35,34,50,
             55,47,46,
             44,45,38,
             40,37,36,
             60,50,54,
             39,41,47)

# estime os efeitos dos fatores 

a <- 32+43+29
b <- 35+34+50
c <- 44+45+38
ab <- 55+47+46
bc <- 60+50+54
ac <- 40+37+36
abc <- 39+41+47
um <- 22+31+25

efeito_a <- (a+ab+ac+abc-um-b-c-bc)/12

efeito_b <- (b+ab+bc+abc-um-a-c-ac)/12

efeito_c <- (c+ac+bc+abc-um-a-b-ab)/12

efeito_ab <- (um-a-b+ab+c-ac-bc+abc)/12

efeito_ac <- (um-a+b-ab-c+ac-bc+abc)/12

efeito_bc <- (um+a-b-ab-c-ac+bc+abc)/12

efeito_abc <- (abc-ac-bc+c-ab+a+b-um)/12

# anova 
fat_a <- as.factor(rep(c(-1,1,-1,1,-1,1,-1,1), each=3))

fat_b <- as.factor(rep(c(-1,-1,1,1,-1,-1,1,1),each=3))
 
fat_c <- as.factor(rep(c(-1,-1,-1,-1,1,1,1,1),each=3))


modelo <- aov(valores~fat_a+fat_b+fat_c+fat_a:fat_b+fat_a:fat_c+fat_b:fat_c+fat_a:fat_b:fat_c)
summary(modelo)

# modelo de regressão
x1 <- rep(c(-1,1,-1,1,-1,1,-1,1), each=3)
x2 <- rep(c(-1,-1,1,1,-1,-1,1,1),each=3)
x3 <- rep(c(-1,-1,-1,-1,1,1,1,1),each=3)
modreg <- lm(valores ~x1*x2*x3)
summary(modreg)
# pressupostos 

## normalidade

residuo <- modelo$residuals

shapiro.test(residuo)

qqnorm(residuo)
qqline(residuo)

# não rejeitamos normalidade a um nível de 5% de confiança

# indepndencia

plot(residuo)

# homocedasticidade

library(car)

leveneTest(valores~fat_a)
leveneTest(valores~fat_b)
leveneTest(valores~fat_c)
leveneTest(valores~fat_a:fat_b)
leveneTest(valores~fat_a:fat_c)
leveneTest(valores~fat_b:fat_c)
leveneTest(valores~fat_a:fat_b:fat_c)

# rejeita homocedasticidade de valores e fator a a um nivel de 5% de significancia

# plot de interação 
# como analiso isso com tres fatores?
interaction.plot(fat_a, fat_b, valores)
interaction.plot(fat_a, fat_c, valores)
interaction.plot(fat_b, fat_a, valores)
interaction.plot(fat_b, fat_c, valores)
interaction.plot(fat_c, fat_a, valores)
interaction.plot(fat_c, fat_b, valores)

# 



