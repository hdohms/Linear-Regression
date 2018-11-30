data("GaltonFamilies")

# Prepare the data set with the heights of fathers and the first son
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# LSE
fit <- lm(son ~ father, data = galton_heights)
summary(fit)

#Question 1
data("Teams")

data_teams <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R / G, BB_per_game = BB / G, HR_per_game = HR / G)

fit <-lm(R_per_game ~ BB_per_game + HR_per_game, data = data_teams)
fit
