data("GaltonFamilies")

ds_theme_set()

# Prepare the data set with the heights of fathers and the first son
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Summarize the data
galton_heights %>% 
  summarize(mean(father), sd(father), mean(son), sd(son))

# Making the plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)