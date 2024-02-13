## code to prepare `data_numeric_regression` dataset goes here
N = 10000;

withr::local_seed(123)
rw = rnorm(N, 0.1, 0.0161812)
r = rlnorm(N, 7.71, 1.0056)
Tu = runif(N, 63070, 115600)
Tl = runif(N, 63.1, 116)
Hu = runif(N, 990, 1100)
Hl = runif(N, 700, 820)
L = runif(N, 1120, 1680)
Kw = runif(N, 9855, 12045)

y = 2*pi*Tu*(Hu-Hl) / log(r/rw) /(1+2*L*Tu/log(r/rw)/(rw^2)/(Kw^2)+Tu/Tl)

data_numeric_regression <- data.frame(y, rw, r, Tu, Tl, Hu, Hl, L, Kw)
usethis::use_data(data_numeric_regression, overwrite = TRUE)
