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

# Q2c

run_simulation <- function(n_trials, n, p, cutoff) {
  pvalues <- vector()
  for (i in 1:n_trials) {
    res <- generate_data(n, p)
    covariates <- res$covariates
    responses <- res$responses
    pvals <- model_select(covariates, responses, cutoff)
    pvalues <- c(pvalues, pvals)
  }
  hist(as.numeric(pvalues))
}

n <- c(100, 1000, 10000)
p <- c(10, 20, 50)
n_trials = 1000
cutoff <- 0.05

res_df <- expand.grid(n, p, n_trials, cutoff)
colnames(res_df) <- c("n", "p", "n_trials", "cutoff")

for (i in 1:nrow(res_df)){
  run_simulation(n_trials = res_df[i, "n_trials"], n = res_df[i, "n"], p = res_df[i, "p"], cutoff = res_df[i, "cutoff"])
}