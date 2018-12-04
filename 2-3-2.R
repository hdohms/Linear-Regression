data("Teams")

# Tibbles display better
as.tibble(Teams)


# Subsets of tibbles are tibbles
class(Teams[,20])
class(as.tibble(Teams[,20]))
class(as.tibble(Teams)$HR)


# Tibbles can have complex entries
tibble(id = c(1, 2, 3), func = c(mean, median, sd))


# Tibbles can be grouped