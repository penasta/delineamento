

dados<- c(28,25,27,36,32,32,18,19,23,31,30,29)
replicas<- factor(rep(1:3,4))
A<- factor(rep(rep(c('1','a'),each=3), 2))
B<- factor(rep(c('1', 'b'), each=6))
AB <- A:B 
resp<- tapply(dados, AB, sum)
resp

df <- data.frame(dados,A,B,AB)

# Efeito A 
(resp["a:b"]- resp["1:b"] + resp["a:1"] - resp["1:1"])/ (2*3)


modelo <-aov(dados~ A*B)
summary(modelo)

## sem interação 

modelo2 <-aov(dados~ A+ B)
summary(modelo2)


## modelo 
facta<- c(rep(rep(c(-1,1),each=3), 2))
factb<- c(rep(c(-1, 1), each=6))

regressão <- lm(dados~facta*factb)
summary(regressão)


library(margins)
persp( regressão, xlab= "A", ylab = "B", zlab= " Valores Ajustados", main="Superfície de Resposta para o ML")


