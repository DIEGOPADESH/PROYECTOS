
u.knn <- "https://www.finley-lab.com/files/data/knnExample.csv"
knnExample <- read.csv(u.knn, header=TRUE)
str(knnExample)

ggplot(data = knnExample, aes(x = x1, y = x2)) + 
  geom_point(aes(color = as.factor(class))) +
  theme_bw()

expand.grid(x = c(1,2), y = c(5,3.4,2))

x.test <- expand.grid(x1 = seq(-2.6, 4.2, by = 0.1), 
                      x2 = seq(-2, 2.9, by = 0.1))


library(class)
Example_knn <- knn(knnExample[,c(1,2)], x.test, knnExample[,3], 
                   k = 15, prob = TRUE)
prob <- attr(Example_knn, "prob")
head(prob)


library(dplyr)
df1 <- mutate(x.test, prob = prob, class = 0,  
              prob_cls = ifelse(Example_knn == class, 1, 0))
str(df1)



df2 <- mutate(x.test, prob = prob, class = 1,  
              prob_cls = ifelse(Example_knn == class, 1, 0))
bigdf <- bind_rows(df1, df2)

names(knnExample)


ggplot(bigdf) + 
  geom_point(aes(x=x1, y =x2, col=class), 
             data = mutate(x.test, class = Example_knn), 
             size = 0.5) + 
  geom_point(aes(x = x1, y = x2, col = as.factor(class)), 
             size = 4, shape = 1, data = knnExample) + 
  geom_contour(aes(x = x1, y = x2, z = prob_cls, 
                   group = as.factor(class), color = as.factor(class)), 
               size = 1, bins = 1, data = bigdf) + theme_bw()
