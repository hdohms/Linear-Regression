data("GaltonFamilies")

# Prepare the data set with the heights of fathers and the first son
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Calculate the sample correlation (n = 25), the outcome is the random variable R
set.seed(0)
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(cor(father, son))
R

# Monte-Carlo simulation of the sample correlation and the plot
B <- 1000
N <- 25
R <- replicate(B,{
  R <- sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>% .$r
})

data.frame(R) %>%
  ggplot(aes(R)) +
  geom_histogram(binwidth = 0.05, color ="black")

mean(R)
sd(R)

## QQ-plot
qqnorm(R); qqline(R)


# Monte-Carlo simulation of the sample correlation and the plot
B <- 1000
N <- 50
R <- replicate(B,{
  R <- sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>% .$r
})

data.frame(R) %>%
  ggplot(aes(R)) +
  geom_histogram(binwidth = 0.05, color ="black")

mean(R)
sd(R)

## QQ-plot
qqnorm(R); qqline(R)
