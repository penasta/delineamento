dados <- c(90,86,96,84,100,92,92,81,
           102,87,106,90,105,97,96,80,
           114,93,112,91,108,95,98,83)
bloco <- factor(rep(rep(1:4,each=2),3))
filtro <- factor(rep(1:2,12))
cluster <- factor(rep(c("Low","Medium","High"),each=8))

df <- data.frame(dados,bloco,filtro,cluster)

anova <- aov(dados ~ bloco + filtro * cluster)
summary(anova)

a <- 3 
b <- 2
n <- 4

shapiro.test(anova$residuals)

pacman::p_load(car)
leveneTest(dados ~ bloco)
leveneTest(dados ~ filtro)
leveneTest(dados ~ cluster)
