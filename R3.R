housing <- read.csv(file.choose(), sep = '.')

housing <- read.csv("C:\\Users\\User\\Desktop\\Новая папка (2)\\housing.csv", sep = "\t")

View(housing)

head(housing)

tail(housing)

str(housing)

summary(housing)

library(tidyverse)

housing <- housing %>% mutate(MEDV_N  = MEDV *1000)

View(housing)

small <- housing %>% filter(MEDV > 40, MEDV < 50)

View(small)

nrow(small)

housing <- housing %>% mutate(MEDV_LOG  = log(MEDV))

View(housing)

housing1 <- housing %>% filter(LSTAT >= 30)
View(housing1)

hist(housing$LSTAT,
     main = 'LSTAT',
     sub = 'LSTAT',
     col = 'darkgreen',
     xlab ='percentage')

dev.copy(png, "histogram.png")
hist(housing$LSTAT,
     main = 'LSTAT',
     sub = 'LSTAT',
     col = 'darkgreen',
     xlab ='percentage')
dev.off()


flats <- read.csv("C:\\Users\\User\\Desktop\\Новая папка (2)\\flats.csv", sep = ",")

View(flats)

library(tidyverse)

flats %>% group_by(brick) %>% tally

flats %>% group_by(brick) %>% summarise(mean(price))


ggplot(data = flats, aes(x = price)) + geom_histogram(binwidth =1.2, fill = 'red', color ='black')+
  facet_grid(~walk)

ggplot(data = flats, aes(x= totsp, y = price, color = walk, size = kitsp)) + geom_point()
