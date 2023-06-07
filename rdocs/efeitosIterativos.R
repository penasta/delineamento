dados <- c(130,155,34,40,20,70,
           74,180,80,75,82,58,
           150,188,136,122,25,70,
           159,126,106,115,58,45,
           138,110,174,120,96,104,
           168,160,150,139,82,60)

material <- factor(rep(1:3,each=12))
temperatura <- rep(factor(c(15,15,70,70,125,125)),6)

df <- data.frame(dados,material,temperatura)

library(pacman)
p_load(tidyverse)

mppp <- mean(dados)

mi1pp <- df %>%
  select(dados,material) %>%
  filter(material == 1) %>%
  summarize(mean(dados))
mi1pp <- mi1pp[1,]

mi2pp <- df %>%
  select(dados,material) %>%
  filter(material == 2) %>%
  summarize(mean(dados))
mi2pp <- mi2pp[1,]

mi3pp <- df %>%
  select(dados,material) %>%
  filter(material == 3) %>%
  summarize(mean(dados))
mi3pp <- mi3pp[1,]



mj1pp <- df %>%
  select(dados,temperatura) %>%
  filter(temperatura == 15) %>%
  summarize(mean(dados)) %>%
  pull()

mj2pp <- df %>%
  select(dados,temperatura) %>%
  filter(temperatura == 70) %>%
  summarize(mean(dados)) %>%
  pull()

mj3pp <- df %>%
  select(dados,temperatura) %>%
  filter(temperatura == 125) %>%
  summarize(mean(dados))%>%
  pull()

df$media <- mppp

df <- df %>%
  mutate(media_i = case_when(material == 1 ~ mi1pp,
                             material == 2 ~ mi2pp,
                             material == 3 ~ mi3pp),
         media_j = case_when(temperatura == 15 ~ mj1pp,
                             temperatura == 70 ~ mj2pp,
                             temperatura == 125 ~ mj3pp))

mi1j1p <- df %>%
  select(dados,material,temperatura) %>%
  filter(material == 1 & temperatura == 15) %>%
  summarise(mean(dados)) %>%
  pull()

mi1j2p <- df %>%
  select(dados,material,temperatura) %>%
  filter(material == 1 & temperatura == 70) %>%
  summarise(mean(dados)) %>%
  pull()

mi1j3p <- df %>%
  select(dados,material,temperatura) %>%
  filter(material == 1 & temperatura == 125) %>%
  summarise(mean(dados)) %>%
  pull()

mi2j1p <- df %>%
  select(dados,material,temperatura) %>%
  filter(material == 2 & temperatura == 15) %>%
  summarise(mean(dados)) %>%
  pull()

mi2j2p <- df %>%
  select(dados,material,temperatura) %>%
  filter(material == 2 & temperatura == 70) %>%
  summarise(mean(dados)) %>%
  pull()

mi2j3p <- df %>%
  select(dados,material,temperatura) %>%
  filter(material == 2 & temperatura == 125) %>%
  summarise(mean(dados)) %>%
  pull()

mi3j1p <- df %>%
  select(dados,material,temperatura) %>%
  filter(material == 3 & temperatura == 15) %>%
  summarise(mean(dados)) %>%
  pull()

mi3j2p <- df %>%
  select(dados,material,temperatura) %>%
  filter(material == 3 & temperatura == 70) %>%
  summarise(mean(dados)) %>%
  pull()

mi3j3p <- df %>%
  select(dados,material,temperatura) %>%
  filter(material == 3 & temperatura == 125) %>%
  summarise(mean(dados)) %>%
  pull()

df <- df %>%
  mutate(taui = media_i-media,
         betaj = media_j-media,
         media_ij = case_when(material == 1 & temperatura == 15 ~ mi1j1p,
                              material == 1 & temperatura == 70 ~ mi1j2p,
                              material == 1 & temperatura == 125 ~ mi1j3p,
                              material == 2 & temperatura == 15 ~ mi2j1p,
                              material == 2 & temperatura == 70 ~ mi2j2p,
                              material == 2 & temperatura == 125 ~ mi2j3p,
                              material == 3 & temperatura == 15 ~ mi3j1p,
                              material == 3 & temperatura == 70 ~ mi3j2p,
                              material == 3 & temperatura == 125 ~ mi3j3p),
         tauibetaj = media_ij-media_i-media_j+media,
         residuo = dados-media_ij)

shapiro.test(df$residuo)
plot(df$residuo)

a <- 3
b <- 3
n <- 4
N <- 36

SQA <- df %>% # ta errado esse
  select(media,media_i) %>%
  summarise(sqa = (1/(b*n)*sum(media_i^2-(media^2/(a*b*n))))) %>%
  pull() 
  
# ...

anova <- aov(dados ~ temperatura*material)
summary(anova)

interaction.plot(temperatura,material,dados)

# Tamanho da amostra

npca <- (b*n*sum(unique(df$taui)^2))/675
ncpb <- (a*n*sum(unique(df$betaj)^2))/675
ncpab <- (n*sum(unique(df$tauibetaj)^2))/675

pchisq(39119/675,2,ncp=0)
pchisq(10684/675,2,ncp=0)

anova2 <- aov(dados ~ temperatura+material)
summary(anova2)

anova(anova,anova2)

# --------------------------------------------------------------------------- #

p_load(effectsize)
options(es.use_symbols = TRUE)

eta_squared(anova)
omega_squared(anova)
epsilon_squared(anova)
cohens_f(anova)
cohens_f_squared(anova)
#eta_squared_posterior(anova)

# --------------------------------------------------------------------------- #

obs <- c(-3,-1,-1,0,-1,1,0,1,0,2,1,1,2,6,3,5,5,7,4,6,7,10,9,11)

n <- 2
a<-3
b<-2
c<-2

fatA<-factor(rep(c("10","12","14"),each=b*c*n))
fatB<-factor(rep(rep(c("25","30"),each=c*n),a))
fatC<-factor(rep(rep(c("200","250"),n),a*c))

dados <- data.frame(obs,fatA,fatB,fatC)

anova <- aov(obs ~ fatA*fatB*fatC)
summary(anova)
