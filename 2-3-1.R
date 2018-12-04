data("Teams")

dat <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = round(HR / G, 1),
         BB_per_game = BB  / G,
         R_per_game  = R) %>%
  select(HR_per_game, BB_per_game, R_per_game) %>%
  filter(HR_per_game >= 0.4 & HR_per_game <= 1.2)

dat %>% group_by(HR_per_game) %>%
  summarize(slope = cor(BB_per_game, R_per_game) * sd(R_per_game) / sd(BB_per_game))

# Estimated slope with function lm() - not correct
# The lm()-finction ignores the group_by()-function
dat %>% group_by(HR_per_game) %>%
  lm(R_per_game ~ BB_per_game, data = .) %>%
  .$coef

# Output: Tibbles
dat %>% group_by(HR_per_game) %>% head()

# Output: Class
dat %>% group_by(HR_per_game) %>% class()