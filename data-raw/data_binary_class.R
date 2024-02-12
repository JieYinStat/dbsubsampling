## code to prepare `data_binary_class` dataset goes here
N <-  10000
d <-  7
sigma_norm <-  matrix(0.5, d, d) + diag(0.5, d)
beta_true <-  matrix(0.5, d, 1)

withr::local_seed(123)
x <- mvtnorm::rmvnorm(N, mean = rep(0, d), sigma = sigma_norm)
y <- rbinom(N, 1, 1 / (1 + exp(-x%*% beta_true)))

data_binary_class <- data.frame(x, "y" = y)
usethis::use_data(data_binary_class, overwrite = TRUE)
