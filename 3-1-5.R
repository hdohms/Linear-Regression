N <- 100
Sigma <- matrix(c(1,0.75,0.75, 1), 2, 2)*1.5
means <- list(c(11,3), c(9,5), c(7,7), c(5,9), c(3,11))
dat <- lapply(means, function(mu) 
  MASS::mvrnorm(N, mu, Sigma))
dat <- tbl_df(Reduce(rbind, dat)) %>% 
  mutate(Z = as.character(rep(seq_along(means), each = N)))
names(dat) <- c("X", "Y", "Z")
dat %>% ggplot(aes(X,Y)) + geom_point(alpha = .5) +
  ggtitle(paste("correlation = ", round(cor(dat$X, dat$Y), 2)))


means <- tbl_df(Reduce(rbind, means)) %>% setNames(c("x","y")) %>%
  mutate(z = as.character(seq_along(means)))
corrs <- dat %>% group_by(Z) %>% summarize(cor = cor(X,Y)) %>% .$cor 
dat %>% ggplot(aes(X, Y, color = Z)) + 
  geom_point(show.legend = FALSE, alpha = 0.5) +
  ggtitle(paste("correlations =",  paste(signif(corrs,2), collapse=" "))) +
  annotate("text", x = means$x, y = means$y, label = paste("Z=", means$z), cex = 5)  


