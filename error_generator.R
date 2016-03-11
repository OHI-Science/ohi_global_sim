## generating simulated data

data <- data.frame(mean = c(100, 1), sd = c(1,1)) %>%
  rowwise() %>%
  mutate(random = rnorm(1, mean, sd))
