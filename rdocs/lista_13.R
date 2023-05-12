dados <- c(122,144,145,125,137,144,120,134,136,150,155,156,153,165,171)
bloco <- factor(rep(1:3,5))
tratamento <- factor(rep(1:5,each=3))
df <- data.frame(dados,bloco,tratamento)

anova <- aov(dados ~ tratamento + bloco, data=df)

summary(anova)

pacman::p_load(tidyverse)

# ----

media_bloco <- df %>%
  select(dados,bloco) %>% 
  group_by(bloco) %>%
  summarise(media_bloco = mean(dados)) %>% 
  pull()

media_tratamento <- df %>%
  select(dados,tratamento) %>% 
  group_by(tratamento) %>%
  summarise(media_tratamento = mean(dados)) %>% 
  pull()

media_geral <- mean(df$dados)

SQb <- sum((media_bloco-media_geral)^2)*5
SQtrat <- sum((media_tratamento-media_geral)^2)*3

df$media_bloco <- rep(media_bloco,5)
df$media_tratamento <- rep(media_tratamento,each=3)
df$media_geral <- rep(media_geral,15)

df <- df %>%
  mutate(residuo = dados - media_bloco - media_tratamento + media_geral)

SQres <- sum((df$residuo)^2)

QMb <- SQb/(max(as.numeric(bloco))-1)
QMtrat <- SQtrat/(max(as.numeric(tratamento))-1)
QMres <- SQres/((max(as.numeric(bloco))-1)*(max(as.numeric(tratamento))-1))

Fbloco <- QMb/QMres
Ftrat <- QMtrat/QMres

pvalortrat <- round(pf(Ftrat,4,8,lower.tail=F),8)
pvalorb <- round(pf(Fbloco,2,8,lower.tail=F),5)

# Montando a tabela
fonte <- c("tratamento","bloco","residuo","total")
SQtotal <- SQtrat+SQb+SQres
SQ <- c(SQtrat,SQb,SQres,SQtotal)
GLtrat <- (max(as.numeric(tratamento))-1)
GLb <- (max(as.numeric(bloco))-1)
GLres <- (max(as.numeric(tratamento))-1)*(max(as.numeric(bloco))-1)
GLtot <- length(dados)-1
GL <- c(GLtrat,GLb,GLres,GLtot)

QM <- c(QMtrat,QMb,QMres,"")

valorf <- c(Ftrat,Fbloco,"","")
pvalor <- c(pvalortrat,pvalorb,"","")

ANOVA <- data.frame(fonte,SQ,GL,QM,valorf,pvalor)
ANOVA
#-----

summary(anova)

# ---- 1.3) Pressupostos:
# Independência
# Normalidade
# Homocedasticidade

# H0: normal. H1: c.c;
shapiro.test(df$residuo)
plot(df$residuo)
qqnorm(df$residuo)
qqline(df$residuo)
# Não rejeitamos H0.

#Homocedasticidade:
bartlett.test(df$dados ~ df$tratamento)
pacman::p_load(lawstat)
levene.test(df$dados,df$tratamento)

pacman::p_load(asbio)
tukey.add.test(dados,tratamento,bloco) # regressão/regressão

#mod <- lm(dados ~ tratamento + bloco)
#ad <- (predict(mod))^2
#modad <- lm(dados ~ tratamento + bloco + ad)
#plot(TukeyHSD(anova))
#bartlett.test(df$residuo)
#plot(df$residuo)

# ---- 1.4)

#QMtotal <- QMb + QMtrat + QMres
#((QMb+QMtrat)/QMtotal)*100
# 98.53% da variabilidade do modelo é explicada.

1-(SQres/SQtotal)
(1-(SQres/SQtotal))*100
# 96.42% da variabilidade é explicada pelo modelo.

# ---- 1.5)

TukeyHSD(anova)

# ----- 1.6)

# P(Não rejeitar H0|h0 falsa)
# ~ 0.53

alpha <- .05
#taui <- unique(df$media_tratamento) - unique(df$media_geral)
taui <- c(-5,0,0,0,5)

sigma2 <- QMres

a <- 5
b <- 3

delta <- b*sum(taui^2/sigma2)
fcrit <- qf(1-alpha,GLtrat,(a-1)*(b-1))
#fcrit <- qf(1-alpha,a-1,N-a)

beta <- pf(fcrit,df1=GLtrat,df2=(a-1)*(b-1),ncp=delta)
beta
poder <- 1-beta
poder

# ---- 1.7)

# Mudando o b

# b = 4
b <- 4

delta <- b*sum(taui^2/sigma2)
fcrit <- qf(1-alpha,GLtrat,(a-1)*(b-1))
#fcrit <- qf(1-alpha,a-1,N-a)

beta <- pf(fcrit,df1=GLtrat,df2=(a-1)*(b-1),ncp=delta)
beta
poder <- 1-beta
poder

#b = 5
b <- 5

delta <- b*sum(taui^2/sigma2)
fcrit <- qf(1-alpha,GLtrat,(a-1)*(b-1))
#fcrit <- qf(1-alpha,a-1,N-a)

beta <- pf(fcrit,df1=GLtrat,df2=(a-1)*(b-1),ncp=delta)
beta
poder <- 1-beta
poder

#b = 6
b <- 6

delta <- b*sum(taui^2/sigma2)
fcrit <- qf(1-alpha,GLtrat,(a-1)*(b-1))
#fcrit <- qf(1-alpha,a-1,N-a)

beta <- pf(fcrit,df1=GLtrat,df2=(a-1)*(b-1),ncp=delta)
beta
poder <- 1-beta
poder

#b = 7
b <- 7

delta <- b*sum(taui^2/sigma2)
fcrit <- qf(1-alpha,GLtrat,(a-1)*(b-1))
#fcrit <- qf(1-alpha,a-1,N-a)

beta <- pf(fcrit,df1=GLtrat,df2=(a-1)*(b-1),ncp=delta)
beta
poder <- 1-beta
poder

# Portanto, para chegar a > 95% de poder, é necessário ao menos 7 blocos.