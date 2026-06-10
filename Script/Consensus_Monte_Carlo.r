# 1. Load and prepare data

accident_data <- read.csv("Dataset\\accident_2013.csv")

# Display first 5 rows of the dataset
head(accident_data, 5)

# Select variables
data_used <- accident_data[, c(
  "road_type",
  "speed_limit",
  "accident_severity"
)]

# Remove missing values if any
data_used <- na.omit(data_used)

# Create binary response variable
data_used$accident_severity_binary <- ifelse(
  data_used$accident_severity == "slight",
  "slight",
  "others"
)

# Convert predictors to factors
data_used$road_type <- factor(data_used$road_type)
data_used$speed_limit <- factor(data_used$speed_limit)

# "others" is the reference class, "slight" is the event being modeled
data_used$accident_severity_binary <- factor(
  data_used$accident_severity_binary,
  levels = c("others", "slight")
)

# Remove the original multiclass severity variable
data_used$accident_severity <- NULL

# Check data
head(data_used, 10)

# Check response distribution
table(data_used$accident_severity_binary)
prop.table(table(data_used$accident_severity_binary))



# 2. Fit frequentist logistic regression

# Remove observations with unknown road type
data_used <- subset(data_used, road_type != "unknown")
data_used$road_type <- droplevels(data_used$road_type)

# Check response distribution after removing unknown road type
table(data_used$accident_severity_binary)
prop.table(table(data_used$accident_severity_binary))

# Fit final frequentist logistic regression
logistic_model <- glm(
  accident_severity_binary ~ .,
  data = data_used,
  family = binomial
)

summary(logistic_model)

# Check estimated coefficient correlation
cov_beta <- vcov(logistic_model)
cor_beta <- cov2cor(cov_beta)

round(cor_beta, 3)



# 3. Prepare response and design matrix

# Recode response variable
y <- ifelse(data_used$accident_severity_binary == "slight", 1, 0)

# Extract design matrix
X <- model.matrix(logistic_model)

# Store parameter names
param_names <- colnames(X)



# 4. Define prior, likelihood, and posterior

# Log-prior
log_prior <- function(beta, prior_sd = 10) {
  sum(dnorm(beta, mean = 0, sd = prior_sd, log = TRUE))
}

# Logistic regression log-likelihood
log_likelihood <- function(beta, y, X) {
  eta <- as.vector(X %*% beta)

  log_one_plus_exp_eta <- ifelse(
    eta > 0,
    eta + log1p(exp(-eta)),
    log1p(exp(eta))
  )

  sum(y * eta - log_one_plus_exp_eta)
}

# Log-posterior
log_posterior <- function(beta, y, X, prior_sd = 10) {
  log_prior(beta, prior_sd = prior_sd) +
    log_likelihood(beta, y, X)
}



# 5. General posterior summary function

posterior_statistics <- function(post_samples, alpha = 0.05) {
  # Function to estimate effective sample size
  ess <- function(x) {
    n <- length(x)

    acf_val <- acf(
      x,
      plot = FALSE,
      lag.max = min(200, n - 1)
    )$acf[-1]

    acf_val <- as.numeric(acf_val)

    sum_rho <- 0

    for (rho in acf_val) {
      if (abs(rho) < 0.05) {
        break
      }

      sum_rho <- sum_rho + rho
    }

    denominator <- 1 + 2 * sum_rho

    if (!is.finite(denominator) || denominator <= 0) {
      return(n)
    }

    min(n, n / denominator)
  }

  # Add parameter names if missing
  if (is.null(colnames(post_samples))) {
    colnames(post_samples) <- paste0("beta_", seq_len(ncol(post_samples)))
  }

  # Posterior summaries
  post_mean <- colMeans(post_samples)
  post_median <- apply(post_samples, 2, median)
  post_sd <- apply(post_samples, 2, sd)

  # Effective sample size and Monte Carlo standard error
  post_ess <- apply(post_samples, 2, ess)
  post_mcse <- post_sd / sqrt(post_ess)

  # Credible interval probabilities
  lower_prob <- alpha / 2
  upper_prob <- 1 - alpha / 2

  lower_ci <- apply(post_samples, 2, quantile, probs = lower_prob)
  upper_ci <- apply(post_samples, 2, quantile, probs = upper_prob)

  # Create summary table
  stats_df <- data.frame(
    Parameter = colnames(post_samples),
    Mean = post_mean,
    Median = post_median,
    Std_Dev = post_sd,
    ESS = post_ess,
    MCSE = post_mcse,
    Lower_95_CI = lower_ci,
    Upper_95_CI = upper_ci,
    row.names = NULL
  )

  return(stats_df)
}



# 6. General histogram and trace plot function

hist_trace_plot <- function(samples,
                            param_names = colnames(samples),
                            params_per_page = 4) {
  # Save current plotting settings
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Add parameter names if missing
  if (is.null(param_names)) {
    param_names <- paste0("beta_", seq_len(ncol(samples)))
  }

  num_params <- length(param_names)
  post_means <- colMeans(samples)

  for (start in seq(1, num_params, by = params_per_page)) {
    end <- min(start + params_per_page - 1, num_params)
    index <- start:end

    # 2 rows:
    # first row = histogram
    # second row = trace plot
    par(mfrow = c(2, length(index)))

    # Histogram plots
    for (i in index) {
      hist(
        samples[, i],
        breaks = 30,
        main = paste("Posterior of", param_names[i]),
        xlab = paste(param_names[i], "values"),
        ylab = "Frequency"
      )

      abline(
        v = post_means[i],
        col = "red",
        lwd = 1.5
      )
    }

    # Trace plots
    for (i in index) {
      plot(
        samples[, i],
        type = "l",
        main = paste("Trace plot of", param_names[i]),
        xlab = "Iteration",
        ylab = paste(param_names[i], "values"),
        lwd = 0.5
      )

      abline(
        h = post_means[i],
        col = "red",
        lwd = 1.5
      )
    }
  }
}



# 7. Random Walk Metropolis-Hastings

set.seed(1)

num_iterations <- 100000
burn_in_fraction <- 0.2

# Number of parameters
d <- length(coef(logistic_model))

# Starting value from frequentist logistic regression
start_value <- coef(logistic_model)

# Replace any non-finite starting values with 0
start_value[!is.finite(start_value)] <- 0

# Tuning factor selected after tuning
rwmh_tuning_factor <- 0.75

# Full covariance random-walk proposal
proposal_cov_rwmh <- rwmh_tuning_factor^2 * (2.38^2 / d) * vcov(logistic_model)
proposal_cov_rwmh <- as.matrix(proposal_cov_rwmh)

# Cholesky decomposition is used to generate correlated multivariate normal proposals
proposal_chol_rwmh <- chol(proposal_cov_rwmh)

# Random walk proposal
random_walk_proposal <- function(current_beta, proposal_chol) {
  as.vector(
    current_beta + t(proposal_chol) %*% rnorm(length(current_beta))
  )
}

rwmh_block <- function(start_value,
                       iterations,
                       y,
                       X,
                       proposal_chol,
                       prior_sd = 10,
                       param_names = NULL) {
  # Create matrix to store MCMC samples
  chain <- matrix(
    NA,
    nrow = iterations + 1,
    ncol = length(start_value)
  )

  # Add parameter names if provided
  if (!is.null(param_names)) {
    colnames(chain) <- param_names
  }

  # Store starting value
  chain[1, ] <- start_value

  # Track number of accepted proposals
  accept_count <- 0

  # Store current beta and current log-posterior
  current <- start_value

  current_log_posterior <- log_posterior(
    beta = current,
    y = y,
    X = X,
    prior_sd = prior_sd
  )

  for (i in seq_len(iterations)) {
    # Generate proposed beta
    proposal <- random_walk_proposal(
      current_beta = current,
      proposal_chol = proposal_chol
    )

    # Calculate log-posterior of proposed beta
    proposal_log_posterior <- log_posterior(
      beta = proposal,
      y = y,
      X = X,
      prior_sd = prior_sd
    )

    # Log acceptance probability
    log_acceptance_prob <- proposal_log_posterior - current_log_posterior

    # Accept or reject
    if (log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_posterior <- proposal_log_posterior
      accept_count <- accept_count + 1
    }

    # Store current beta
    chain[i + 1, ] <- current
  }

  acceptance_rate <- accept_count / iterations

  cat("Random Walk MH Acceptance Rate:", acceptance_rate, "\n")

  return(
    list(
      chain = chain,
      acceptance_rate = acceptance_rate
    )
  )
}

# Run Random Walk MH
rwmh_result <- rwmh_block(
  start_value = start_value,
  iterations = num_iterations,
  y = y,
  X = X,
  proposal_chol = proposal_chol_rwmh,
  prior_sd = 10,
  param_names = param_names
)

# Extract the full Random Walk MH chain
rwmh_chain <- rwmh_result$chain

# Calculate burn-in
burn_in <- round(num_iterations * burn_in_fraction)

# Remove burn-in
rwmh_post_samples <- rwmh_chain[-seq_len(burn_in + 1), , drop = FALSE]

# Posterior summary table
rwmh_post_stats_table <- posterior_statistics(rwmh_post_samples)

print(rwmh_post_stats_table)

# Odds ratio summary
rwmh_post_stats_table$Odds_Ratio <- exp(rwmh_post_stats_table$Mean)
rwmh_post_stats_table$Lower_95_OR <- exp(rwmh_post_stats_table$Lower_95_CI)
rwmh_post_stats_table$Upper_95_OR <- exp(rwmh_post_stats_table$Upper_95_CI)

print(rwmh_post_stats_table)

# Histogram and trace plots
hist_trace_plot(rwmh_post_samples)



# 8. Independent Metropolis-Hastings

# Proposal mean from frequentist logistic regression
proposal_mean_imh <- coef(logistic_model)
proposal_mean_imh[!is.finite(proposal_mean_imh)] <- 0

# Tuning factor for independent proposal
imh_tuning_factor <- 1.5

# Independent proposal covariance
proposal_cov_imh <- imh_tuning_factor^2 * vcov(logistic_model)
proposal_cov_imh <- as.matrix(proposal_cov_imh)

# Cholesky decomposition is used to generate correlated multivariate normal proposals
proposal_chol_imh <- chol(proposal_cov_imh)

# Function to calculate log density of multivariate normal proposal
log_dmvnorm_mahalanobis <- function(x, mean, cov_matrix) {
  # Number of parameters
  d <- length(x)

  # Mahalanobis distance squared:
  # (x - mean)' Sigma^{-1} (x - mean)
  mahalanobis_distance_sq <- mahalanobis(
    x = x,
    center = mean,
    cov = cov_matrix
  )

  # Log determinant of covariance matrix: log |Sigma|
  log_det <- determinant(
    cov_matrix,
    logarithm = TRUE
  )$modulus[1]

  # Multivariate normal log-density
  -0.5 * (
    d * log(2 * pi) +
      log_det +
      mahalanobis_distance_sq
  )
}

# Independent proposal function
independent_proposal <- function(proposal_mean, proposal_chol) {
  as.vector(
    proposal_mean + t(proposal_chol) %*% rnorm(length(proposal_mean))
  )
}

imh_block <- function(start_value,
                      iterations,
                      y,
                      X,
                      proposal_mean,
                      proposal_cov,
                      proposal_chol,
                      prior_sd = 10,
                      param_names = NULL) {
  # Create matrix to store MCMC samples
  chain <- matrix(
    NA,
    nrow = iterations + 1,
    ncol = length(start_value)
  )

  # Add parameter names if provided
  if (!is.null(param_names)) {
    colnames(chain) <- param_names
  }

  # Store starting value
  chain[1, ] <- start_value

  # Track number of accepted proposals
  accept_count <- 0

  # Store current beta and current log-posterior
  current <- start_value

  current_log_posterior <- log_posterior(
    beta = current,
    y = y,
    X = X,
    prior_sd = prior_sd
  )

  # Store proposal density of current value
  current_log_proposal_density <- log_dmvnorm_mahalanobis(
    x = current,
    mean = proposal_mean,
    cov_matrix = proposal_cov
  )

  for (i in seq_len(iterations)) {
    # Generate proposed beta from fixed proposal distribution
    proposal <- independent_proposal(
      proposal_mean = proposal_mean,
      proposal_chol = proposal_chol
    )

    # Calculate log-posterior of proposed beta
    proposal_log_posterior <- log_posterior(
      beta = proposal,
      y = y,
      X = X,
      prior_sd = prior_sd
    )

    # Calculate proposal density of proposed beta
    proposal_log_proposal_density <- log_dmvnorm_mahalanobis(
      x = proposal,
      mean = proposal_mean,
      cov_matrix = proposal_cov
    )

    # Independent MH log acceptance probability
    log_acceptance_prob <-
      proposal_log_posterior -
      current_log_posterior +
      current_log_proposal_density -
      proposal_log_proposal_density

    # Accept or reject
    if (log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_posterior <- proposal_log_posterior
      current_log_proposal_density <- proposal_log_proposal_density
      accept_count <- accept_count + 1
    }

    # Store current beta
    chain[i + 1, ] <- current
  }

  acceptance_rate <- accept_count / iterations

  cat("Independent MH Acceptance Rate:", acceptance_rate, "\n")

  return(
    list(
      chain = chain,
      acceptance_rate = acceptance_rate
    )
  )
}

# Run Independent MH
imh_result <- imh_block(
  start_value = start_value,
  iterations = num_iterations,
  y = y,
  X = X,
  proposal_mean = proposal_mean_imh,
  proposal_cov = proposal_cov_imh,
  proposal_chol = proposal_chol_imh,
  prior_sd = 10,
  param_names = param_names
)

# Extract the full Independent MH chain
imh_chain <- imh_result$chain

# Remove burn-in
imh_post_samples <- imh_chain[-seq_len(burn_in + 1), , drop = FALSE]

# Posterior summary table
imh_post_stats_table <- posterior_statistics(imh_post_samples)

print(imh_post_stats_table)

# Odds ratio summary
imh_post_stats_table$Odds_Ratio <- exp(imh_post_stats_table$Mean)
imh_post_stats_table$Lower_95_OR <- exp(imh_post_stats_table$Lower_95_CI)
imh_post_stats_table$Upper_95_OR <- exp(imh_post_stats_table$Upper_95_CI)

print(imh_post_stats_table)

# Histogram and trace plots
hist_trace_plot(imh_post_samples)



# 9. Consensus Monte Carlo using Random Walk MH

# Number of data subsets
cmc_num_subsets <- 4

# Number of MCMC iterations for each subset chain
cmc_iterations <- 50000
cmc_burn_in <- round(cmc_iterations * burn_in_fraction)

# Randomly split observations into subsets
set.seed(123)

cmc_subset_id <- sample(
  rep(seq_len(cmc_num_subsets), length.out = nrow(X))
)

# Subposterior log-density
# The prior is divided by the number of subsets so that
# multiplying all subposteriors gives the full posterior.
log_subposterior <- function(beta,
                             y_subset,
                             X_subset,
                             prior_sd = 10,
                             num_subsets = 4) {
  (1 / num_subsets) * log_prior(beta, prior_sd = prior_sd) +
    log_likelihood(beta, y_subset, X_subset)
}

# Random Walk MH sampler for one subset
rwmh_subset_block <- function(start_value,
                              iterations,
                              y_subset,
                              X_subset,
                              proposal_chol,
                              prior_sd = 10,
                              num_subsets = 4,
                              param_names = NULL) {
  # Create matrix to store MCMC samples
  chain <- matrix(
    NA,
    nrow = iterations + 1,
    ncol = length(start_value)
  )

  # Add parameter names if provided
  if (!is.null(param_names)) {
    colnames(chain) <- param_names
  }

  # Store starting value
  chain[1, ] <- start_value

  # Track accepted proposals
  accept_count <- 0

  # Store current beta and current log-subposterior
  current <- start_value

  current_log_subposterior <- log_subposterior(
    beta = current,
    y_subset = y_subset,
    X_subset = X_subset,
    prior_sd = prior_sd,
    num_subsets = num_subsets
  )

  for (i in seq_len(iterations)) {
    # Random walk proposal
    proposal <- random_walk_proposal(
      current_beta = current,
      proposal_chol = proposal_chol
    )

    # Log-subposterior of proposed beta
    proposal_log_subposterior <- log_subposterior(
      beta = proposal,
      y_subset = y_subset,
      X_subset = X_subset,
      prior_sd = prior_sd,
      num_subsets = num_subsets
    )

    # Random Walk MH acceptance probability
    # No proposal-density correction is needed because the proposal is symmetric
    log_acceptance_prob <-
      proposal_log_subposterior -
      current_log_subposterior

    # Accept or reject
    if (log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_subposterior <- proposal_log_subposterior
      accept_count <- accept_count + 1
    }

    # Store current beta
    chain[i + 1, ] <- current
  }

  acceptance_rate <- accept_count / iterations

  return(
    list(
      chain = chain,
      acceptance_rate = acceptance_rate
    )
  )
}

# Proposal covariance for subset Random Walk MH
# Each subset posterior is wider than the full posterior,
# so the covariance is multiplied by cmc_num_subsets.
cmc_rwmh_tuning_factor <- 0.75

cmc_rwmh_proposal_cov <-
  cmc_rwmh_tuning_factor^2 *
  (2.38^2 / d) *
  cmc_num_subsets *
  vcov(logistic_model)

cmc_rwmh_proposal_cov <- as.matrix(cmc_rwmh_proposal_cov)

# Cholesky decomposition is used to generate correlated multivariate normal proposals
cmc_rwmh_proposal_chol <- chol(cmc_rwmh_proposal_cov)

# Store subset chains and acceptance rates
cmc_rwmh_subset_samples_list <- vector("list", cmc_num_subsets)
cmc_rwmh_acceptance_rates <- numeric(cmc_num_subsets)

# Run Random Walk MH on each subset
for (k in seq_len(cmc_num_subsets)) {
  cat("\nRunning Consensus RWMH subset", k, "of", cmc_num_subsets, "\n")

  subset_rows <- which(cmc_subset_id == k)

  y_subset <- y[subset_rows]
  X_subset <- X[subset_rows, , drop = FALSE]

  subset_result <- rwmh_subset_block(
    start_value = start_value,
    iterations = cmc_iterations,
    y_subset = y_subset,
    X_subset = X_subset,
    proposal_chol = cmc_rwmh_proposal_chol,
    prior_sd = 10,
    num_subsets = cmc_num_subsets,
    param_names = param_names
  )

  cmc_rwmh_acceptance_rates[k] <- subset_result$acceptance_rate

  subset_chain <- subset_result$chain

  cmc_rwmh_subset_samples_list[[k]] <-
    subset_chain[-seq_len(cmc_burn_in + 1), , drop = FALSE]

  cat(
    "Subset", k,
    "RWMH acceptance rate:",
    cmc_rwmh_acceptance_rates[k],
    "\n"
  )
}

# Function to combine subposterior samples using Consensus Monte Carlo
combine_consensus_samples <- function(subposterior_samples_list, param_names) {
  # Use the same number of samples from each subset
  num_consensus_samples <- min(
    sapply(subposterior_samples_list, nrow)
  )

  subposterior_samples_list <- lapply(
    subposterior_samples_list,
    function(samples) {
      samples[seq_len(num_consensus_samples), , drop = FALSE]
    }
  )

  # Estimate subposterior covariance matrices
  subposterior_cov_list <- lapply(
    subposterior_samples_list,
    cov
  )

  # Convert covariance matrices to precision matrices
  # Precision matrix = inverse covariance matrix
  subposterior_precision_list <- lapply(
    subposterior_cov_list,
    solve
  )

  # Total precision matrix
  total_precision <- Reduce(
    "+",
    subposterior_precision_list
  )

  # Consensus covariance matrix
  consensus_cov <- solve(total_precision)

  # Precision-weighted sum of subset samples
  weighted_sum <- matrix(
    0,
    nrow = num_consensus_samples,
    ncol = ncol(subposterior_samples_list[[1]])
  )

  for (k in seq_along(subposterior_samples_list)) {
    weighted_sum <-
      weighted_sum +
      subposterior_samples_list[[k]] %*%
        subposterior_precision_list[[k]]
  }

  consensus_samples <- weighted_sum %*% consensus_cov

  colnames(consensus_samples) <- param_names

  return(consensus_samples)
}

# Combine Random Walk MH subposterior samples
cmc_rwmh_samples <- combine_consensus_samples(
  subposterior_samples_list = cmc_rwmh_subset_samples_list,
  param_names = param_names
)

# Posterior summary for Consensus Monte Carlo using RWMH
cmc_rwmh_post_stats_table <- posterior_statistics(cmc_rwmh_samples)

cmc_rwmh_post_stats_table$Odds_Ratio <-
  exp(cmc_rwmh_post_stats_table$Mean)

cmc_rwmh_post_stats_table$Lower_95_OR <-
  exp(cmc_rwmh_post_stats_table$Lower_95_CI)

cmc_rwmh_post_stats_table$Upper_95_OR <-
  exp(cmc_rwmh_post_stats_table$Upper_95_CI)

print(cmc_rwmh_post_stats_table)

# Histogram and trace plots
hist_trace_plot(cmc_rwmh_samples)



# 10. Consensus Monte Carlo using Independent MH

# Proposal mean from the full-data frequentist logistic regression
cmc_imh_proposal_mean <- coef(logistic_model)
cmc_imh_proposal_mean[!is.finite(cmc_imh_proposal_mean)] <- 0

# Tuning factor for Independent MH subset proposal
cmc_imh_tuning_factor <- 1.5

# Each subset posterior is wider than the full posterior,
# so the covariance is multiplied by cmc_num_subsets.
cmc_imh_proposal_cov <-
  cmc_imh_tuning_factor^2 *
  cmc_num_subsets *
  vcov(logistic_model)

cmc_imh_proposal_cov <- as.matrix(cmc_imh_proposal_cov)

# Cholesky decomposition is used to generate correlated multivariate normal proposals
cmc_imh_proposal_chol <- chol(cmc_imh_proposal_cov)

# Independent MH sampler for one subset
imh_subset_block <- function(start_value,
                             iterations,
                             y_subset,
                             X_subset,
                             proposal_mean,
                             proposal_cov,
                             proposal_chol,
                             prior_sd = 10,
                             num_subsets = 4,
                             param_names = NULL) {
  # Create matrix to store MCMC samples
  chain <- matrix(
    NA,
    nrow = iterations + 1,
    ncol = length(start_value)
  )

  # Add parameter names if provided
  if (!is.null(param_names)) {
    colnames(chain) <- param_names
  }

  # Store starting value
  chain[1, ] <- start_value

  # Track accepted proposals
  accept_count <- 0

  # Store current beta and current log-subposterior
  current <- start_value

  current_log_subposterior <- log_subposterior(
    beta = current,
    y_subset = y_subset,
    X_subset = X_subset,
    prior_sd = prior_sd,
    num_subsets = num_subsets
  )

  # Proposal density of current beta
  current_log_proposal_density <- log_dmvnorm_mahalanobis(
    x = current,
    mean = proposal_mean,
    cov_matrix = proposal_cov
  )

  for (i in seq_len(iterations)) {
    # Independent proposal from a fixed multivariate normal distribution
    proposal <- independent_proposal(
      proposal_mean = proposal_mean,
      proposal_chol = proposal_chol
    )

    # Log-subposterior of proposed beta
    proposal_log_subposterior <- log_subposterior(
      beta = proposal,
      y_subset = y_subset,
      X_subset = X_subset,
      prior_sd = prior_sd,
      num_subsets = num_subsets
    )

    # Proposal density of proposed beta
    proposal_log_proposal_density <- log_dmvnorm_mahalanobis(
      x = proposal,
      mean = proposal_mean,
      cov_matrix = proposal_cov
    )

    # Independent MH acceptance probability
    # Proposal-density correction is required
    log_acceptance_prob <-
      proposal_log_subposterior -
      current_log_subposterior +
      current_log_proposal_density -
      proposal_log_proposal_density

    # Accept or reject
    if (log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_subposterior <- proposal_log_subposterior
      current_log_proposal_density <- proposal_log_proposal_density
      accept_count <- accept_count + 1
    }

    # Store current beta
    chain[i + 1, ] <- current
  }

  acceptance_rate <- accept_count / iterations

  return(
    list(
      chain = chain,
      acceptance_rate = acceptance_rate
    )
  )
}

# Store subset chains and acceptance rates
cmc_imh_subset_samples_list <- vector("list", cmc_num_subsets)
cmc_imh_acceptance_rates <- numeric(cmc_num_subsets)

# Run Independent MH on each subset
for (k in seq_len(cmc_num_subsets)) {
  cat("\nRunning Consensus IMH subset", k, "of", cmc_num_subsets, "\n")

  subset_rows <- which(cmc_subset_id == k)

  y_subset <- y[subset_rows]
  X_subset <- X[subset_rows, , drop = FALSE]

  subset_result <- imh_subset_block(
    start_value = start_value,
    iterations = cmc_iterations,
    y_subset = y_subset,
    X_subset = X_subset,
    proposal_mean = cmc_imh_proposal_mean,
    proposal_cov = cmc_imh_proposal_cov,
    proposal_chol = cmc_imh_proposal_chol,
    prior_sd = 10,
    num_subsets = cmc_num_subsets,
    param_names = param_names
  )

  cmc_imh_acceptance_rates[k] <- subset_result$acceptance_rate

  subset_chain <- subset_result$chain

  cmc_imh_subset_samples_list[[k]] <-
    subset_chain[-seq_len(cmc_burn_in + 1), , drop = FALSE]

  cat(
    "Subset", k,
    "IMH acceptance rate:",
    cmc_imh_acceptance_rates[k],
    "\n"
  )
}

# Combine Independent MH subposterior samples
cmc_imh_samples <- combine_consensus_samples(
  subposterior_samples_list = cmc_imh_subset_samples_list,
  param_names = param_names
)

# Posterior summary for Consensus Monte Carlo using IMH
cmc_imh_post_stats_table <- posterior_statistics(cmc_imh_samples)

cmc_imh_post_stats_table$Odds_Ratio <-
  exp(cmc_imh_post_stats_table$Mean)

cmc_imh_post_stats_table$Lower_95_OR <-
  exp(cmc_imh_post_stats_table$Lower_95_CI)

cmc_imh_post_stats_table$Upper_95_OR <-
  exp(cmc_imh_post_stats_table$Upper_95_CI)

print(cmc_imh_post_stats_table)

# Histogram and trace plots
hist_trace_plot(cmc_imh_samples)