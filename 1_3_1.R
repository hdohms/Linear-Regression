data("GaltonFamilies")

# Prepare the data set with the heights of fathers and the first son
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Calculate the conditional average height of the sun, if the father is 72 inch
conditional_avg <- galton_heights %>%
  filter(round(father) == 72 ) %>%
  summarize(avg = mean(son)) %>%
  .$avg
conditional_avg

# Stratification
galton_heights %>%
  mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# Another example
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(conditional_avg_son = mean(son)) %>%
  ggplot(aes(father, conditional_avg_son)) +
  geom_point()

# And now with regression line
r <- galton_heights %>%
  summarize(r = cor(father, son)) %>%
  .$r
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)

# Last plot with regression line
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * (s_y / s_x)
b <- mu_y - (m * mu_x)

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

galton_heights %>%
  ggplot(aes(scale(father), scale(son))) + #standard units
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)