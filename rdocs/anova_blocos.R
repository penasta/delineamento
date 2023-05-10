library(pacman)
p_load(tidyverse)

i1 <- c(73,68,74,71,67)
i2 <- c(73,67,75,72,70)
i3 <- c(75,68,78,73,68)
i4 <- c(73,71,75,75,69)

df <- as_tibble(t(data.frame(i1,i2,i3,i4)))
df$produto <- c(1:4)

dfl <- df %>% pivot_longer(!produto)
dfl$produto <- factor(dfl$produto)
dfl$name <- factor(dfl$name)

anova <- aov(value ~ produto + name, data=dfl)
summary(anova)

# tentando fazer na mão

mpp <- mean(c(i1,i2,i3,i4))

sqb <- NULL
sqb[1] <- (mean(i1)-mpp)^2
sqb[2] <- (mean(i2)-mpp)^2
sqb[3] <- (mean(i3)-mpp)^2
sqb[4] <- (mean(i4)-mpp)^2

SQb <- sum(sqb)*5

sqt <- NULL
sqt[1] <- (mean(df$V1)-mpp)^2
sqt[2] <- (mean(df$V2)-mpp)^2
sqt[3] <- (mean(df$V3)-mpp)^2
sqt[4] <- (mean(df$V4)-mpp)^2
sqt[5] <- (mean(df$V5)-mpp)^2

SQt <- sum(sqt)*4

SQRes <- sum(anova[[2]]^2)

# OU: 
# sum(unlist(lapply(df[, 1:5], function(x) (mean(x) - mpp)^2))) * 4

# Quadrados médios:

MSt <- SQt/(5-1) # g.l.
MSb <- SQb/(4-1) # g.l.
MSRes <- SQRes/(20-(5-1)-(4-1)-1) # g.l.

# Calculando a estatística F
Valorf <- round(MSb/MSRes,3)
Valorf2 <- round(MSt/MSRes,3)

# Calculando o p-valor

pvalor <- round(pf(Valorf,3,12,lower.tail=F),3)
pvalor2 <- pf(Valorf2,4,12,lower.tail=F)


# Montando a ANOVA manual
ANOVA <- data.frame(Fonte = c("Produto","Rolo de tecido","Residuos","Total"),
                    SQ = c(SQb,SQt,SQRes,sum(SQb,SQt,SQRes)),
                    GL = c(3,4,12,19),
                    QM = c(round(MSb,2),MSt,round(MSRes,2),""),
                    ValorF = c(Valorf,Valorf2,"",""),
                    Pvalor = c(pvalor,round(pvalor2,7),"","")
                    )

# Comparando com a ANOVA do R
summary(anova)
ANOVA

# --------------------------------------------------------------------------- #
# 1.3) ----

j1 <- c(73,73,75,73)
j2 <- c(68,67,68,71)
j3 <- c(74,75,78,75)
j4 <- c(71,72,73,75)
j5 <- c(67,70,68,69)

mediatrat <- c(mean(i1)-mpp,mean(i2)-mpp,mean(i3)-mpp,mean(i4)-mpp)
mediabloco <- c(mean(j1)-mpp,mean(j2)-mpp,mean(j3)-mpp,mean(j4)-mpp,mean(j5)-mpp)

residuosdf <- data.frame(rep(mpp,20),rep(mediabloco,4),rep(mediatrat,each=5),c(i1,i2,i3,i4))

colnames(residuosdf) <- c("mediag","mediatrat","mediabloco","obs")

residuosdf <- residuosdf %>%
  mutate(residuos = obs - mediag - mediatrat - mediabloco, fitted = mediag+mediatrat+mediabloco)
residuosdf$residuos
anova$residuals

shapiro.test(residuosdf$residuos)
plot(residuosdf$residuos)

# O teste de Shapiro rejeita a normalidade dos erros. Portanto, um pressuposto não foi atendido.

# --------------------------------------------------------------------------- #
# 1.4) ----

ANOVA
1-21.8/(12.95+157+21.8)

# Portanto, cerca de 96% da variação é explicada pelo modelo.

# --------------------------------------------------------------------------- #
# 1.5) ----

TukeyHSD(anova)

# Nenhum par é diferente

# --------------------------------------------------------------------------- #
# 1.6) ----

alpha <- .05
taui <- c(-1.5,0,0,1.5)
sigma2 <- 1.82 # QMRes

delta <- 5*sum(taui^2)/sigma2

a <- 4
N <- 13
n <- 5

fcrit <- qf(1-alpha,4-1,(4-1)*(4-1))
fcrit <- qf(1-alpha,a-1,N-a)

beta <- pf(fcrit,df1=a-1,df2=a*n-a,ncp=delta)
poder <- 1-beta
poder






