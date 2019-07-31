# Q2

# Q2a

generate_data <- function(n, p) {
  covariates <- matrix(data = rnorm(n = n, mean = 0, sd = 1), nrow = n, ncol = p)
  responses <- as.vector(rnorm(n = n, mean = 0, sd = 1))
  return(list(covariates = covariates, responses = responses))
}