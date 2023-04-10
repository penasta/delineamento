um <- c(575,565,600,725)
dois <- c(542,593,651,700)
tres <- c(530,590,610,715)
quatro <- c(539,579,637,685)
cinco <- c(570,610,629,710)
potencia <- c(160,180,200,220)
total <- c(2756,2937,3127,3535)

library(pacman)
p_load(tidyverse)

df <- data.frame(potencia,um,dois,tres,quatro,cinco,total)

colnames(df) <- c("potencia","1","2","3","4","5","total")

