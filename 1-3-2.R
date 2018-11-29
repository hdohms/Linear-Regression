data("GaltonFamilies")

# Prepare the data set with the heights of fathers and the first son
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# bivariate normal distribution
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample = son)) +
  facet_wrap(~z_father)