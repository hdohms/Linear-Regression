data("Teams")

Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles = ( H - HR - X2B - X3B) / G, BB = BB / G, HR = HR / G) %>%
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))
