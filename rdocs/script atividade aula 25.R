# LEITURA DOS VALORES

valores = c(22,32,35,55,44,40,60,39,
            31,43,34,47,45,37,50,41,
            25,29,50,46,38,36,54,47)
fator_A = c(rep(c(-1,1),4))
fator_A = rep(fator_A,3)
fator_B = c(rep(c(-1,-1,1,1),2))
fator_B = rep(fator_B,3)
fator_C = c(rep(-1,4),rep(1,4))
fator_C = rep(fator_C,3)
Dados = as.data.frame(valores)
Dados$fator_A = fator_A
Dados$fator_B = fator_B
Dados$fator_C = fator_C
Dados

## Número de fatores
k <- 3
## Número de níveis
a <- b <- c <- 2
## Número de repetições
r <- 3


tab.sinais <- cbind(I = 1, Dados[, c("fator_A", "fator_B", "fator_C")])
tab.sinais <- transform(tab.sinais,
                        AB = Dados$fator_A*Dados$fator_B,
                        AC = Dados$fator_A*Dados$fator_C,
                        BC = Dados$fator_B*Dados$fator_C,
                        ABC = Dados$fator_A*Dados$fator_B*Dados$fator_C)
names(tab.sinais) <- c("(1)", "a", "b", "c", "ab", "ac", "bc", "abc")
tab.sinais

#calculo do contraste___________________________________________________________
(contr <- t(tab.sinais[, -1]) %*% Dados$valores)

# calculo do efeito_____________________________________________________________
(ef <- contr/(r * 2^(k-1)))


# os maiores efeitos aparentes são dos níveis: b, c , ac



# vendo como as interações reagem_______________________________________________
with(Dados, interaction.plot(fator_A, fator_B, valores))
# vendo como as interações reagem
with(Dados, interaction.plot(fator_A, fator_C, valores))
# vendo como as interações reagem
with(Dados, interaction.plot(fator_B, fator_C, valores))

# CALCULO DAS SOMAS DE QUADRADOS________________________________________________
(SQ <- contr^2/(r * 2^k))


# SOMA DE QUADRADO TOTAL E RESIDUO

n = 24
## SQTot
(SQTot <- sum(Dados$valores^2) - (sum(Dados$valores)^2/n))

## SQRes
(SQRes <- SQTot - sum(SQ))


# CALCULO DOS GRAUS DE LIBERDADE________________________________________________

(glA <- a - 1)#1
(glB <- b - 1)#1
(glC <- c - 1)#1
(glAB <- (a-1)*(b-1))#1
(glAC <- (a-1)*(c-1))#1
(glBC <- (b-1)*(c-1))#1
(glABC <- (a-1)*(b-1)*(c-1))#1
(glRES <- a*b*c*(r-1))#16



# CALCULO DO QUADRADO MEDIO_____________________________________________________

(MQ <- SQ/c(glA, glB, glC, glAB, glAC, glBC, glABC))

## Resíduo
(MQRes <- SQRes/glRES)


# CONSTRUINDO TABELA ANOVA______________________________________________________

## Valores F
f <- MQ/MQRes

## Valores p
p <- pf(f, df1 = glA, df2 = glRES, lower.tail = FALSE)
p

## Tabela de ANOVA
tab.anova <- data.frame("GL" = c(glA, glB, glC, glAB, glAC, glBC, glABC, glRES),
                        "SQ" = c(SQ, SQRes),
                        "QM" = c(MQ, MQRes),
                        "F" = c(f, NA),
                        "p-valor" = c(p, NA),
                        row.names = c("A", "B", "C", "A:B", "A:C",
                                      "B:C", "A:B:C", "Resíduo"))
tab.anova

# Aqui confirmamos que os níveis que realmente aparentam ter os maiores impactos
# foram os que "apitaram" no pvalor sendo, novamente, os níveis: B, C, AC



# MODELO DE REGRESSAO___________________________________________________________


## Modelo usando lm()
m0 <- lm(valores ~ fator_A * fator_B * fator_C, data = Dados)
summary(m0)

# analise de residuos
shapiro.test(m0$residuals)# normal

plot(m0$residuals)# indep

#modelo comparativo:
m1 <- lm(valores ~ fator_A * fator_C + fator_B + fator_C, data = Dados)
summary(m1)


# GRAFICOS DE INTERAçÂO_________________________________________________________

## Gráficos de interação
par(mfrow = c(1, 3))
with(Dados, {
  interaction.plot(fator_A, fator_B, valores)
  interaction.plot(fator_A, fator_C, valores)
  interaction.plot(fator_B, fator_C, valores)
})

# install.packages("lattice")  # Instalar o pacote
pacman::p_load(lattice)            # Carregar o pacote

par(mfrow = c(1, 1))
# INTERATIVOS
levelplot(valores ~ fator_A * fator_B, data = Dados, cuts = 90,
          col.regions = heat.colors)

levelplot(valores ~ fator_A * fator_C, data = Dados, cuts = 90,
          col.regions = heat.colors)

levelplot(valores ~ fator_B * fator_C, data = Dados, cuts = 90,
          col.regions = heat.colors)


# -1A, 1B e 1C são os níveis que devemos usar


par(mfrow = c(1, 1))
# install.packages("dae")
# library(dae)
## Usando o pacote dae
# dae::interaction.ABC.plot(valores, fator_A, fator_B, fator_C, data = Dados)
# nao consegui entender o porque de usar isso
