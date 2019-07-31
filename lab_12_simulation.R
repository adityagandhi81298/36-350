# Q2

# Q2a

generate_data <- function(n, p) {
  covariates <- matrix(data = rnorm(n = (n * p), mean = 0, sd = 1), nrow = n, ncol = p)
  responses <- as.vector(rnorm(n = n, mean = 0, sd = 1))
  return(list(covariates = covariates, responses = responses))
}

# Q2b

model_select <- function(covariates, responses, cutoff) {
  reg_m <- lm(responses ~ covariates)
  p_values <- summary(reg_m)$coefficients[, "Pr(>|t|)"][-1]
  index_p <- which(p_values <= cutoff)
  if (length(index_p) == 0){
    return(vector())
  } else {
    new_reg <- lm(responses ~ covariates[, index_p])
    return(summary(new_reg)$coefficients[, "Pr(>|t|)"][-1])
  }
}