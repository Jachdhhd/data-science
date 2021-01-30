setwd("C:/Users/User/Desktop/Новая папка (2)/")
parfume <- read.csv("parfume.csv")
View(parfume)

parfume1 <- parfume[parfume$vote < 2,]
View(parfume1)

library(tidyverse)

barplot(table(parfume1[parfume1$group == 'control',]$vote))

barplot(table(parfume1[parfume1$group == 'test',]$vote))

ggplot(data = parfume1, aes(x=vote)) + geom_histogram( binwidth = 0.5,
                                              fill ='skyblue', color ='violet') +
  facet_grid(~group)


test <- parfume1[parfume1$group == 'test',]$vote

control <- parfume1[parfume1$group == 'control',]$vote


p_control <- sum(control) / length(control) # конверсия 1
p_test <- sum(test) / length(test) # конверсия 2
p_test - p_control

N <- 1000 

set.seed(123)

differences <- rep(NA, N) # 1000 пустых элементов
for(i in 1:N){
  s1 <- sample(control,replace=TRUE) 
  s2 <- sample(test,replace=TRUE) 
  p1 <- sum(s1)/length(s1) 
  p2 <- sum(s2)/length(s2) 
  p_diff <- p2 - p1 
  differences[i] <- p_diff 
}


dif_cent <- differences - mean(differences)

sum(differences - mean(differences)> p_test - p_control)

#P_value = 0 <0.05,  отвергаем нулевую гипотезу. Тестовый вариант лучше контрольного 
