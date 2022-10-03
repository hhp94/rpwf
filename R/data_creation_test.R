library(devtools)
usethis::use_testthat()
usethis::use_github(private = TRUE)

# Generate the data needed for testing
rpwf_sim = function(n_train = 100, n_test = 10, seed = 1234) {
  set.seed(seed)
  n = n_train + n_test
  df = data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    target = rbinom(n, size = 1, prob = 0.5),
    id = seq_len(n)
  )
  return(list(train = df[1:n_train,], test = df[(n_train + 1):n,]))
}
