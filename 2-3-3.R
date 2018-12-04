data("Teams")

dat <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = round(HR / G, 1),
         BB_per_game = BB  / G,
         R_per_game  = R / G) %>%
  select(HR_per_game, BB_per_game, R_per_game) %>%
  filter(HR_per_game >= 0.4 & HR_per_game <= 1.2)

dat %>%
  group_by(HR_per_game) %>%
  do(fit = lm(R_per_game ~ BB_per_game, data = .))

# Error, because do() expecting a data frame as output
dat %>%
  group_by(HR_per_game) %>%
  do(lm(R_per_game ~ BB_per_game, data = .))

# Output as data frame
get_slope <- function(data){
  fit <- lm(R_per_game ~ BB_per_game, data = data)
  data.frame(slope = fit$coefficient[2],
             se = summary(fit)$coefficient[2, 2])
}

dat %>%
  group_by(HR_per_game) %>%
  do(get_slope(.))


# Name the output
dat %>%
  group_by(HR_per_game) %>%
  do(slope = get_slope(.))


# If data frame being returned has more than more one row
get_lse <- function(data){
  fit <- lm(R_per_game ~ BB_per_game, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficient,
             se = summary(fit)$coefficient[, 2])
}

dat %>%
  group_by(HR_per_game) %>%
  do(get_lse(.))