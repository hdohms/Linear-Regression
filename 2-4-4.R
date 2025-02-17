falling_object <- rfalling_object()
falling_object

falling_object %>% 
  ggplot(aes(time, observed_distance)) + 
  geom_point() +
  ylab("Distance in meters") + 
  xlab("Time in seconds")

fit <- falling_object %>% 
  mutate(time_sq = time^2) %>% 
  lm(observed_distance~time+time_sq, data=.)
tidy(fit)

augment(fit) %>% 
  ggplot() +
  geom_point(aes(time, observed_distance)) + 
  geom_line(aes(time, .fitted), col = "blue")

tidy(fit, conf.int = TRUE)
