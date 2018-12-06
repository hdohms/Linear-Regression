N <- 25
G <- 1000000
sim_data <- tibble(group = rep(1:G, each = N), X = rnorm(N*G), Y = rnorm(N*G))

res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(X, Y)) %>% 
  arrange(desc(r))
head(res)

sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(X, Y)) +
  geom_point() + 
  geom_smooth(method = "lm")

res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y ~ X, data = .)))