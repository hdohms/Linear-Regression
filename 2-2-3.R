data("GaltonFamilies")

# Prepare the data set with the heights of fathers and the first son
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Monte-Carlo simulation and plotting them
B <- 1000
N <- 50
lse <- replicate(B,{
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son~father, data = .) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

p1 <- lse %>%
  ggplot(aes(beta_0)) +
  geom_histogram(binwidth = 5, color = "black")

p2 <- lse %>%
  ggplot(aes(beta_1)) +
  geom_histogram(binwidth = 0.1, color = "black")

grid.arrange(p1, p2, ncol = 2)

# Standard error for one simulated data set
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son~father, data = .) %>% summary

# Estimated standard errors for the lse-data set(see above)
lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))