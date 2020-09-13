library(vtreat)
library(GGally)
library(gridExtra)
library(tidyverse)
set.seed(503)

diamonds_test <- diamonds %>% mutate(diamond_id = row_number()) %>% 
  group_by(cut, color, clarity) %>% sample_frac(0.2) %>% ungroup()

diamonds_train <- anti_join(diamonds %>% mutate(diamond_id = row_number()), 
                            diamonds_test, by = "diamond_id")

diamonds_train
dim(diamonds_train)
# Bir bakalim icine 
glimpse(diamonds)
diamonds %>% summarise_all(funs(sum(is.na(.)))) # bos hucre yok 

plotdata <- diamonds_train %>% select(-x,-y, -z, -diamond_id)
ggplot(plotdata) + geom_point(aes(x=carat, y=price)) + geom_smooth(method = "lm", aes(x=carat, y=price), se=T)

ggplot(plotdata, aes(x=carat, y=price),fill=cut) + geom_point()

ggplot(aes(x=carat, y=price), data=diamonds) +
  geom_point(fill=I("#F79420"), color=I("black"), shape=21) +
  stat_smooth(method = 'lm')
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99)) ) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)) ) +
  ggtitle("Diamonds: Price vs. Carat")
  
  ggplot(aes(x=carat, y=price), data=diamonds) +
    geom_point(fill=I("#F79420"), color=I("black"), shape=21) +
    stat_smooth(method="lm") +
    scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99)) ) +
    scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)) ) +
    ggtitle("Diamonds: Price vs. Carat")
  
  p1 <- ggplot(diamonds_train, aes(x = carat, y = price, color = cut)) + 
    geom_point(alpha = 0.3) + scale_y_log10() 
  p1
  
  p2 <- ggplot(diamonds_train, aes(x = carat, y = price, color = color)) + geom_point(alpha = 0.3) + scale_y_log10() 
  p2
  
  p3 <- ggplot(diamonds_train, aes(x = carat, y = price, color = color)) + geom_point(alpha = 0.3) + scale_y_sqrt() 
  p3
