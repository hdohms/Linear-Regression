set.seed(1)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

tibble(x,y) %>% ggplot(aes(x,y)) + geom_point(alpha = 0.5)

cor(x,y)

cor(x[-23], y[-23])

tibble(x,y) %>% 
  ggplot(aes(rank(x),rank(y))) + 
  geom_point(alpha = 0.5)

cor(rank(x), rank(y))

cor(x, y, method = "spearman")