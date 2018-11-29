data("GaltonFamilies")

# Prepare the data set with the heights of fathers and the first son
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Caluclate the means, standard deviations, and correlation coefficient
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)

# Intercept and slope of the father, given the son
m_y <- r * (s_y / s_x)
b_y <- mu_y - (m_y * mu_x)

# Intercept and slope of the son, given the father
m_x <- r * (s_x / s_y)
b_x <- mu_x - (m_x * mu_y)

# Plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(color='blue', intercept = b_y, slope = m_y) + # Father
  geom_abline(color='green', intercept = b_x, slope = m_x)  # Son
