data("Teams")

dat <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = round(HR / G, 1),
         BB_per_game = BB  / G,
         R_per_game  = R / G) %>%
  select(HR_per_game, BB_per_game, R_per_game) %>%
  filter(HR_per_game >= 0.4 & HR_per_game <= 1.2)

fit <- lm(R_per_game ~ BB_per_game, data = dat)
tidy(fit)

# Additional important summaries
tidy(fit, conf.int = TRUE)

# Outcome is a data frame , we can use do()-function
dat %>%
  group_by(HR_per_game) %>%
  do(tidy(lm(R_per_game ~ BB_per_game, data = .), conf.int = TRUE))


# Select() and filter()
dat %>%
  group_by(HR_per_game) %>%
  do(tidy(lm(R_per_game ~ BB_per_game, data = .), conf.int = TRUE)) %>%
  filter(term == "BB_per_game") %>%
  select(HR_per_game, estimate, conf.low, conf.high)

## Plot
dat %>%
  group_by(HR_per_game) %>%
  do(tidy(lm(R_per_game ~ BB_per_game, data = .), conf.int = TRUE)) %>%
  filter(term == "BB_per_game") %>%
  select(HR_per_game, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR_per_game, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()


## Glance() and Augement()
glance(fit)
augment(fit)
