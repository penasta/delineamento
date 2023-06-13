# 1.0) ----
# Dados
dados <- c(83,77,80,83,85,
           80,85,85,81,79,
           82,97,76,84,76,
           81,93,81,91,83,
           74,87,89,88,72)

livro <- c("A3","A1","A4","A5","A2",
           "A1","A5","A2","A4","A3",
           "A2","A3","A5","A1","A4",
           "A4","A2","A1","A3","A5",
           "A5","A4","A3","A2","A1")

estudante <- rep(c("C1","C2","C3","C4","C5"),each=5)

musica <- rep(c("B1","B2","B3","B4","B5"),5)

df <- data.frame(dados,estudante,livro,musica)

# 1.1) ----
# hipóteses:
# h0) O efeito dos livros é igual
# h1) Ao menos um dos livros difere

# 1.2) ----
anova <- aov(dados ~ estudante+livro+musica)
summary(anova)

  # 1.3) ----
# Pressupostos:

# Normalidade:
qqnorm(anova$residuals)
qqline(anova$residuals)
shapiro.test(anova$residuals)

# Homocedasticidade:
bartlett.test(dados ~ livro)

bartlett.test(dados ~ estudante)
bartlett.test(dados ~ musica)

# Independência:
plot(anova$residuals,anova$fitted.values)

# Aditividade:
aditividade <- lm(dados ~ estudante+livro+musica)
v_adt <- (predict(aditividade))^2
modelo <- lm(dados ~ estudante+livro+musica+v_adt)

anova2 <- anova(aditividade,modelo)
anova2

pacman::p_load(additivityTests)
tukey.test(df)

# 1.4) ----
# Estimativa dos parâmetros:
# mu = 
mu <- mean(dados)
mu

# alpha_i (linha)
pacman::p_load(tidyverse)
df %>%
  select(dados,estudante) %>%
  group_by(estudante) %>%
  summarize(média = mean(dados)-mu)

# tau_j (tratamento)
df %>%
  select(dados,livro) %>%
  group_by(livro) %>%
  summarize(média = mean(dados)-mu)

# beta_k (coluna)
df %>%
  select(dados,musica) %>%
  group_by(musica) %>%
  summarize(média = mean(dados)-mu)

# 1.5) ----
# Diferenças:
boxplot(dados~livro)
TukeyHSD(anova)
