# 1. Data preprocessing

accident_data <- read.csv(
  file.path("Dataset", "accident_2013.csv")
)

# Display first 5 rows of the dataset
head(accident_data, 5)

# Select variables used in the analysis
data_used <- accident_data[, c(
  "road_type",
  "speed_limit",
  "accident_severity"
)]

# Remove missing values if any
data_used <- na.omit(data_used)

# Create binary response variable
# serious_fatal combines serious and fatal accidents
data_used$accident_severity_binary <- ifelse(
  data_used$accident_severity == "slight",
  "slight",
  "serious_fatal"
)

# Convert predictors to factors
data_used$road_type <- factor(data_used$road_type)
data_used$speed_limit <- factor(data_used$speed_limit)

# "slight" is the reference class,
# "serious_fatal" is the event being modeled
data_used$accident_severity_binary <- factor(
  data_used$accident_severity_binary,
  levels = c("slight", "serious_fatal")
)

# Remove the original multiclass severity variable
data_used$accident_severity <- NULL

# Check data before removing unknown road type
head(data_used, 5)

# Check response distribution before removing unknown road type
table(data_used$accident_severity_binary)
prop.table(table(data_used$accident_severity_binary))

# Check predictor distributions before removing unknown road type
table(data_used$road_type)
prop.table(table(data_used$road_type))

table(data_used$speed_limit)
prop.table(table(data_used$speed_limit))

# Remove observations with unknown road type
# This category is removed because it is not an interpretable road type
data_used <- subset(data_used, road_type != "unknown")
data_used$road_type <- droplevels(data_used$road_type)

# Set meaningful reference categories
# The reference categories are chosen because they are the most common groups
data_used$road_type <- relevel(
  data_used$road_type,
  ref = "single carriageway"
)

data_used$speed_limit <- relevel(
  data_used$speed_limit,
  ref = "2"
)

# Check response distribution after removing unknown road type
table(data_used$accident_severity_binary)
prop.table(table(data_used$accident_severity_binary))

# Check predictor distributions after removing unknown road type
table(data_used$road_type)
prop.table(table(data_used$road_type))

table(data_used$speed_limit)
prop.table(table(data_used$speed_limit))



# 2. Fit frequentist logistic regression

# Fit frequentist logistic regression
logistic_model <- glm(
  accident_severity_binary ~ .,
  data = data_used,
  family = binomial
)

# Summary of the fitted logistic regression model
summary(logistic_model)

# Odds ratios
odds_ratio_table <- data.frame(
  Parameter = names(coef(logistic_model)),
  Odds_Ratio = exp(coef(logistic_model)),
  row.names = NULL
)

print(odds_ratio_table)

# Model significance using likelihood-ratio test
g_square <- logistic_model$null.deviance - logistic_model$deviance
g_square_df <- logistic_model$df.null - logistic_model$df.residual
p_value <- pchisq(g_square, g_square_df, lower.tail = FALSE)

print(
  data.frame(
    G_Square = g_square,
    DF = g_square_df,
    P_Value = p_value
  )
)

# Standardized Pearson residuals
pearson_resid_std <- rstandard(logistic_model, type = "pearson")

acf(
  pearson_resid_std,
  main = "Correlogram of Pearson Residuals"
)

# Standardized deviance residuals
deviance_resid_std <- rstandard(logistic_model, type = "deviance")

acf(
  deviance_resid_std,
  main = "Correlogram of Deviance Residuals"
)

# Multicollinearity check
gvif_values <- car::vif(logistic_model)
print(gvif_values)

# Estimated coefficient covariance and correlation
# Used later as a reference covariance matrix for MH proposal distributions.
cov_beta <- vcov(logistic_model)
cor_beta <- cov2cor(cov_beta)

round(cor_beta, 3)



# 3. Prepare response and design matrix

# Recode response variable
# serious_fatal is coded as 1 because it is the event being modeled
y <- ifelse(data_used$accident_severity_binary == "serious_fatal", 1, 0)

# Extract design matrix from the frequentist logistic regression model
X <- model.matrix(logistic_model)

# Store parameter names
param_names <- colnames(X)



# 4. Define prior, likelihood, and posterior

# Cauchy prior scale
prior_scale_value <- 2.5

# Log-prior
# Each coefficient is assigned a Cauchy(0, prior_scale) prior.
log_prior <- function(beta, prior_scale = prior_scale_value) {
  sum(dcauchy(beta, location = 0, scale = prior_scale, log = TRUE))
}

# Logistic regression log-likelihood
log_likelihood <- function(beta, y, X) {
  eta <- as.vector(X %*% beta)

  log_one_plus_exp_eta <- numeric(length(eta))

  positive_eta <- eta > 0

  log_one_plus_exp_eta[positive_eta] <-
    eta[positive_eta] + log1p(exp(-eta[positive_eta]))

  log_one_plus_exp_eta[!positive_eta] <-
    log1p(exp(eta[!positive_eta]))

  sum(y * eta - log_one_plus_exp_eta)
}

# Log-posterior
log_posterior <- function(beta, y, X, prior_scale = prior_scale_value) {
  log_prior(beta, prior_scale = prior_scale) +
    log_likelihood(beta, y, X)
}



# 5. General posterior summary function

posterior_statistics <- function(post_samples, alpha = 0.05) {
  # Function to estimate effective sample size
  ess <- function(x, cap_at_n = TRUE) {
    x <- as.numeric(x)
    x <- x[is.finite(x)]

    n <- length(x)

    if (n < 3) {
      return(n)
    }

    # Degenerate chain: ESS is not meaningful
    if (stats::var(x) == 0) {
      return(NA_real_)
    }

    # Autocorrelation values
    acf_values <- as.numeric(
      stats::acf(
        x,
        plot = FALSE,
        lag.max = n - 1
      )$acf
    )

    # Remove lag 0 autocorrelation
    rho <- acf_values[-1]

    # Number of complete autocorrelation pairs:
    # Gamma_k = rho_{2k - 1} + rho_{2k}
    n_pairs <- floor(length(rho) / 2)

    if (n_pairs < 1) {
      return(n)
    }

    gamma <- rho[2 * seq_len(n_pairs) - 1] +
      rho[2 * seq_len(n_pairs)]

    # Initial positive sequence:
    # stop once paired autocorrelation becomes non-positive
    first_nonpositive <- which(gamma <= 0)[1]

    if (!is.na(first_nonpositive)) {
      if (first_nonpositive == 1) {
        return(n)
      }

      gamma <- gamma[seq_len(first_nonpositive - 1)]
    }

    if (length(gamma) == 0) {
      return(n)
    }

    # Initial monotone sequence:
    # force paired terms to be non-increasing
    if (length(gamma) >= 2) {
      for (i in 2:length(gamma)) {
        if (gamma[i] > gamma[i - 1]) {
          gamma[i] <- gamma[i - 1]
        }
      }
    }

    # Integrated autocorrelation time:
    # tau = 1 + 2 * sum_{t >= 1} rho_t
    tau <- 1 + 2 * sum(gamma)

    if (!is.finite(tau) || tau <= 0) {
      return(n)
    }

    ess_val <- n / tau

    if (cap_at_n) {
      ess_val <- min(n, ess_val)
    }

    ess_val
  }

  # Add parameter names if missing
  if (is.null(colnames(post_samples))) {
    colnames(post_samples) <- paste0("beta_", seq_len(ncol(post_samples)))
  }

  # Posterior summaries on log-odds scale
  post_mean <- colMeans(post_samples)
  post_median <- apply(post_samples, 2, median)
  post_sd <- apply(post_samples, 2, sd)

  # Effective sample size and Monte Carlo standard error
  post_ess <- apply(post_samples, 2, ess)
  post_mcse <- post_sd / sqrt(post_ess)

  # Credible interval probabilities
  lower_prob <- alpha / 2
  upper_prob <- 1 - alpha / 2

  lower_cri <- apply(post_samples, 2, quantile, probs = lower_prob)
  upper_cri <- apply(post_samples, 2, quantile, probs = upper_prob)

  # Create summary table
  stats_df <- data.frame(
    Parameter = colnames(post_samples),
    Mean = post_mean,
    Median = post_median,
    Std_Dev = post_sd,
    ESS = post_ess,
    MCSE = post_mcse,
    CrI_95 = paste0(
      "[",
      round(lower_cri, 4),
      ", ",
      round(upper_cri, 4),
      "]"
    ),
    Odds_Ratio = exp(post_mean),
    row.names = NULL
  )

  return(stats_df)
}



# 6. Functions

hist_trace_plot <- function(samples,
                            param_names = colnames(samples),
                            params_per_page = 2) {
  # Save current plotting settings
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Add parameter names if missing
  if (is.null(param_names)) {
    param_names <- paste0("beta_", seq_len(ncol(samples)))
  }

  num_params <- length(param_names)
  post_means <- colMeans(samples, na.rm = TRUE)

  for (start in seq(1, num_params, by = params_per_page)) {
    end <- min(start + params_per_page - 1, num_params)
    index <- start:end

    # Two rows:
    # first row = posterior histogram
    # second row = trace plot
    par(mfrow = c(2, length(index)))

    # Histogram plots
    for (i in index) {
      hist(
        samples[, i],
        breaks = 30,
        main = paste("Posterior histogram of", param_names[i]),
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

# ACF plot function for MCMC posterior samples
acf_mcmc_plot <- function(samples,
                          param_names = colnames(samples),
                          params_per_page = 2) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  if (is.null(param_names)) {
    param_names <- paste0("beta_", seq_len(ncol(samples)))
  }

  num_params <- length(param_names)

  for (start in seq(1, num_params, by = params_per_page)) {
    end <- min(start + params_per_page - 1, num_params)
    index <- start:end

    par(mfrow = c(1, length(index)))

    for (i in index) {
      acf(
        samples[, i],
        main = paste("ACF plot of", param_names[i]),
        xlab = "Lag",
        ylab = "Autocorrelation"
      )
    }
  }
}

# Density plot function for Consensus Monte Carlo samples
consensus_density_plot <- function(samples,
                                   param_names = colnames(samples),
                                   params_per_page = 2) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  if (is.null(param_names)) {
    param_names <- paste0("beta_", seq_len(ncol(samples)))
  }

  num_params <- length(param_names)
  post_means <- colMeans(samples, na.rm = TRUE)

  for (start in seq(1, num_params, by = params_per_page)) {
    end <- min(start + params_per_page - 1, num_params)
    index <- start:end

    par(mfrow = c(1, length(index)))

    for (i in index) {
      plot(
        density(samples[, i]),
        main = paste("CMC posterior density of", param_names[i]),
        xlab = paste(param_names[i], "values"),
        ylab = "Density",
        lwd = 1.5
      )

      abline(
        v = post_means[i],
        col = "red",
        lwd = 1.5
      )
    }
  }
}

# Optimized mahalanobis distance function for a fixed covariance matrix
mahalanobis_opt <- function(mean, cov_matrix) {
  d <- length(mean)

  cov_chol <- chol(cov_matrix)
  cov_precision <- chol2inv(cov_chol)
  log_det <- 2 * sum(log(diag(cov_chol)))

  function(x) {
    centered_x <- x - mean

    mahalanobis_distance_sq <- as.numeric(
      t(centered_x) %*% cov_precision %*% centered_x
    )

    -0.5 * (
      d * log(2 * pi) +
        log_det +
        mahalanobis_distance_sq
    )
  }
}

# Store runtimes
runtime_table <- data.frame(
  Method = character(),
  Runtime_Seconds = numeric(),
  Runtime_Minutes = numeric()
)



# 7. Full-data RWMH

set.seed(1)

num_iterations <- 100000
burn_in_fraction <- 0.2
burn_in <- round(num_iterations * burn_in_fraction)

# Number of parameters
d <- length(coef(logistic_model))

# Starting value from frequentist logistic regression
start_value <- coef(logistic_model)

# Replace any non-finite starting values with 0
start_value[!is.finite(start_value)] <- 0

# Tuning factor selected after tuning
rwmh_tuning_factor <- 0.75

# Full covariance random-walk proposal
proposal_cov_rwmh <-
  rwmh_tuning_factor^2 *
  (2.38^2 / d) *
  vcov(logistic_model)

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
                       prior_scale = prior_scale_value,
                       param_names = NULL) {
  # Create matrix to store MCMC samples
  chain <- matrix(
    NA_real_,
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
    prior_scale = prior_scale
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
      prior_scale = prior_scale
    )

    # Random Walk MH log acceptance probability
    # No proposal-density correction is required because the proposal is symmetric.
    log_acceptance_prob <-
      proposal_log_posterior -
      current_log_posterior

    # Accept or reject
    if (is.finite(log_acceptance_prob) &&
        log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_posterior <- proposal_log_posterior
      accept_count <- accept_count + 1
    }

    # Store current beta
    chain[i + 1, ] <- current
  }

  acceptance_rate <- accept_count / iterations

  cat("Full-data RWMH acceptance rate:", acceptance_rate, "\n")

  return(
    list(
      chain = chain,
      acceptance_rate = acceptance_rate
    )
  )
}

# Start runtime measurement
rwmh_start_time <- Sys.time()

# Run Full-data RWMH
rwmh_result <- rwmh_block(
  start_value = start_value,
  iterations = num_iterations,
  y = y,
  X = X,
  proposal_chol = proposal_chol_rwmh,
  prior_scale = prior_scale_value,
  param_names = param_names
)

# End runtime measurement
rwmh_end_time <- Sys.time()

rwmh_runtime <- as.numeric(
  difftime(rwmh_end_time, rwmh_start_time, units = "secs")
)

runtime_table <- rbind(
  runtime_table,
  data.frame(
    Method = "Full-data RWMH",
    Runtime_Seconds = rwmh_runtime,
    Runtime_Minutes = rwmh_runtime / 60
  )
)

# Extract the Full-data RWMH chain
rwmh_chain <- rwmh_result$chain

# Remove burn-in
rwmh_post_samples <- rwmh_chain[-seq_len(burn_in + 1), , drop = FALSE]

# Posterior summary table
rwmh_post_stats_table <- posterior_statistics(rwmh_post_samples)

print(rwmh_post_stats_table)

# Histogram and trace plots
hist_trace_plot(rwmh_post_samples)

# ACF plots
acf_mcmc_plot(rwmh_post_samples)

# Print runtime so far
print(runtime_table)



# 8. Full-data IMH

set.seed(2)

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

# Independent proposal function
independent_proposal <- function(proposal_mean, proposal_chol) {
  as.vector(
    proposal_mean + t(proposal_chol) %*% rnorm(length(proposal_mean))
  )
}

# Precompute the fixed independent proposal log-density function
log_proposal_density_imh <- mahalanobis_opt(
  mean = proposal_mean_imh,
  cov_matrix = proposal_cov_imh
)

imh_block <- function(start_value,
                      iterations,
                      y,
                      X,
                      proposal_mean,
                      proposal_chol,
                      log_proposal_density,
                      prior_scale = prior_scale_value,
                      param_names = NULL) {
  # Create matrix to store MCMC samples
  chain <- matrix(
    NA_real_,
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
    prior_scale = prior_scale
  )

  # Store proposal density of current value
  current_log_proposal_density <- log_proposal_density(current)

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
      prior_scale = prior_scale
    )

    # Calculate proposal density of proposed beta
    proposal_log_proposal_density <- log_proposal_density(proposal)

    # Independent MH log acceptance probability
    # Proposal-density correction is required because the proposal is not symmetric.
    log_acceptance_prob <-
      proposal_log_posterior -
      current_log_posterior +
      current_log_proposal_density -
      proposal_log_proposal_density

    # Accept or reject
    if (is.finite(log_acceptance_prob) &&
        log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_posterior <- proposal_log_posterior
      current_log_proposal_density <- proposal_log_proposal_density
      accept_count <- accept_count + 1
    }

    # Store current beta
    chain[i + 1, ] <- current
  }

  acceptance_rate <- accept_count / iterations

  cat("Full-data IMH acceptance rate:", acceptance_rate, "\n")

  return(
    list(
      chain = chain,
      acceptance_rate = acceptance_rate
    )
  )
}

# Start runtime measurement
imh_start_time <- Sys.time()

# Run Full-data IMH
imh_result <- imh_block(
  start_value = start_value,
  iterations = num_iterations,
  y = y,
  X = X,
  proposal_mean = proposal_mean_imh,
  proposal_chol = proposal_chol_imh,
  log_proposal_density = log_proposal_density_imh,
  prior_scale = prior_scale_value,
  param_names = param_names
)

# End runtime measurement
imh_end_time <- Sys.time()

imh_runtime <- as.numeric(
  difftime(imh_end_time, imh_start_time, units = "secs")
)

runtime_table <- rbind(
  runtime_table,
  data.frame(
    Method = "Full-data IMH",
    Runtime_Seconds = imh_runtime,
    Runtime_Minutes = imh_runtime / 60
  )
)

# Extract the Full-data IMH chain
imh_chain <- imh_result$chain

# Remove burn-in
imh_post_samples <- imh_chain[-seq_len(burn_in + 1), , drop = FALSE]

# Posterior summary table
imh_post_stats_table <- posterior_statistics(imh_post_samples)

print(imh_post_stats_table)

# Histogram and trace plots
hist_trace_plot(imh_post_samples)

# ACF plots
acf_mcmc_plot(imh_post_samples)

# Print runtime comparison so far
print(runtime_table)



# 9. Parallel CMC-RWMH

library(parallel)

set.seed(3)

# Number of shards
# K = 3 is chosen to divide the computation into three shards while keeping each shard large enough
# for stable logistic regression inference.
cmc_num_shards <- 3

# Number of MCMC iterations for each shard chain
# Use the same number of iterations as the full-data MCMC chains for comparability.
cmc_iterations <- num_iterations
cmc_burn_in <- round(cmc_iterations * burn_in_fraction)

# Randomly split observations into shards
cmc_shard_id <- sample(
  rep(seq_len(cmc_num_shards), length.out = nrow(X))
)

# Check shard balance
table(cmc_shard_id)

# Check response distribution within each shard
table(cmc_shard_id, y)

# Check predictor distributions within each shard
table(cmc_shard_id, data_used$road_type)
table(cmc_shard_id, data_used$speed_limit)


# Subposterior log-density
# Each shard uses 1 / K of the prior so that multiplying all
# subposteriors gives the full posterior structure.
log_subposterior <- function(beta,
                             y_subset,
                             X_subset,
                             prior_scale,
                             num_subsets) {
  (1 / num_subsets) * log_prior(beta, prior_scale = prior_scale) +
    log_likelihood(beta, y_subset, X_subset)
}


# RWMH sampler for one shard
rwmh_subset_block <- function(start_value,
                              iterations,
                              y_subset,
                              X_subset,
                              proposal_chol,
                              prior_scale,
                              num_subsets,
                              param_names = NULL) {
  # Create matrix to store MCMC samples
  chain <- matrix(
    NA_real_,
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
    prior_scale = prior_scale,
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
      prior_scale = prior_scale,
      num_subsets = num_subsets
    )

    # Random Walk MH acceptance probability
    # No proposal-density correction is needed because the proposal is symmetric.
    log_acceptance_prob <-
      proposal_log_subposterior -
      current_log_subposterior

    # Accept or reject
    if (is.finite(log_acceptance_prob) &&
        log(runif(1)) < log_acceptance_prob) {
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


# Proposal covariance for shard RWMH
# Each shard contains roughly 1 / K of the full dataset.
# Therefore, each subposterior is expected to be wider than the full posterior.
# The full-data covariance estimate is multiplied by K to approximate this.
cmc_rwmh_tuning_factor <- 0.75

cmc_rwmh_proposal_cov <-
  cmc_rwmh_tuning_factor^2 *
  (2.38^2 / d) *
  cmc_num_shards *
  vcov(logistic_model)

cmc_rwmh_proposal_cov <- as.matrix(cmc_rwmh_proposal_cov)

# Cholesky decomposition is used to generate correlated multivariate normal proposals
cmc_rwmh_proposal_chol <- chol(cmc_rwmh_proposal_cov)


# Start runtime measurement
cmc_rwmh_start_time <- Sys.time()


# Safe core detection
available_cores <- detectCores()

if (is.na(available_cores)) {
  available_cores <- 2
}

num_cores <- max(
  1,
  min(cmc_num_shards, available_cores - 1)
)

cat(
  "\nRunning Parallel CMC-RWMH shard chains across",
  num_cores,
  "cores...\n"
)


# Create cluster
cl <- makeCluster(num_cores)

# Run shard chains in parallel and always close the cluster afterward
parallel_rwmh_results <- tryCatch(
  {
    # Reproducible random numbers across parallel workers
    clusterSetRNGStream(cl, iseed = 3)

    # Export required objects and functions to worker processes
    clusterExport(
      cl,
      varlist = c(
        "X",
        "y",
        "cmc_shard_id",
        "rwmh_subset_block",
        "log_subposterior",
        "log_prior",
        "log_likelihood",
        "random_walk_proposal",
        "cmc_rwmh_proposal_chol",
        "cmc_iterations",
        "cmc_burn_in",
        "prior_scale_value",
        "cmc_num_shards",
        "start_value",
        "param_names"
      ),
      envir = environment()
    )

    parLapply(
      cl,
      seq_len(cmc_num_shards),
      function(k) {
        subset_rows <- which(cmc_shard_id == k)

        subset_result <- rwmh_subset_block(
          start_value = start_value,
          iterations = cmc_iterations,
          y_subset = y[subset_rows],
          X_subset = X[subset_rows, , drop = FALSE],
          proposal_chol = cmc_rwmh_proposal_chol,
          prior_scale = prior_scale_value,
          num_subsets = cmc_num_shards,
          param_names = param_names
        )

        subset_chain <- subset_result$chain

        post_burn_in_chain <-
          subset_chain[-seq_len(cmc_burn_in + 1), , drop = FALSE]

        return(
          list(
            chain = post_burn_in_chain,
            acceptance_rate = subset_result$acceptance_rate
          )
        )
      }
    )
  },
  finally = {
    stopCluster(cl)
  }
)


# Extract shard chains and acceptance rates
cmc_rwmh_subset_samples_list <- lapply(
  parallel_rwmh_results,
  function(result) result$chain
)

cmc_rwmh_acceptance_rates <- sapply(
  parallel_rwmh_results,
  function(result) result$acceptance_rate
)

for (k in seq_len(cmc_num_shards)) {
  cat(
    "Shard", k,
    "Parallel CMC-RWMH acceptance rate:",
    cmc_rwmh_acceptance_rates[k],
    "\n"
  )
}


# Function to combine subposterior samples using Consensus Monte Carlo
combine_consensus_samples <- function(subposterior_samples_list, param_names) {
  # Use the same number of post-burn-in samples from each shard
  num_consensus_samples <- min(
    sapply(subposterior_samples_list, nrow)
  )

  num_params <- ncol(subposterior_samples_list[[1]])

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
  # W_k = Sigma_k^{-1}
  subposterior_precision_list <- lapply(
    subposterior_cov_list,
    solve
  )

  # Total precision matrix
  # W = sum_k W_k
  total_precision <- Reduce(
    "+",
    subposterior_precision_list
  )

  # Consensus covariance matrix
  consensus_cov <- solve(total_precision)

  # Precision-weighted sum:
  # sum_k W_k theta_ki
  weighted_sum <- matrix(
    0,
    nrow = num_params,
    ncol = num_consensus_samples
  )

  for (k in seq_along(subposterior_samples_list)) {
    weighted_sum <-
      weighted_sum +
      subposterior_precision_list[[k]] %*%
      t(subposterior_samples_list[[k]])
  }

  # Consensus samples:
  # theta_i^CMC = (sum_k W_k)^(-1) sum_k W_k theta_ki
  consensus_samples <- t(consensus_cov %*% weighted_sum)

  colnames(consensus_samples) <- param_names

  return(consensus_samples)
}


# Combine Parallel CMC-RWMH subposterior samples
cmc_rwmh_samples <- combine_consensus_samples(
  subposterior_samples_list = cmc_rwmh_subset_samples_list,
  param_names = param_names
)


# End runtime measurement
cmc_rwmh_end_time <- Sys.time()

cmc_rwmh_runtime <- as.numeric(
  difftime(cmc_rwmh_end_time, cmc_rwmh_start_time, units = "secs")
)

runtime_table <- rbind(
  runtime_table,
  data.frame(
    Method = "Parallel CMC-RWMH",
    Runtime_Seconds = cmc_rwmh_runtime,
    Runtime_Minutes = cmc_rwmh_runtime / 60
  )
)

# Posterior summary for Parallel CMC-RWMH
cmc_rwmh_post_stats_table <- posterior_statistics(cmc_rwmh_samples)

print(cmc_rwmh_post_stats_table)

# Density plots for Consensus Monte Carlo samples
# These are combined consensus samples, not one single Markov chain.
consensus_density_plot(cmc_rwmh_samples)


# 10. Parallel CMC-IMH

set.seed(4)

# Tuning factor for shard-specific IMH proposal
# A value of 1.5 is used to match the Full-data IMH tuning factor.
cmc_imh_tuning_factor <- 1.5


# IMH sampler for one shard
imh_subset_block <- function(start_value,
                             iterations,
                             y_subset,
                             X_subset,
                             proposal_mean,
                             proposal_chol,
                             log_proposal_density,
                             prior_scale,
                             num_subsets,
                             param_names = NULL) {
  # Create matrix to store MCMC samples
  chain <- matrix(
    NA_real_,
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
    prior_scale = prior_scale,
    num_subsets = num_subsets
  )

  # Proposal density of current beta
  current_log_proposal_density <- log_proposal_density(current)

  for (i in seq_len(iterations)) {
    # Independent proposal from a shard-specific multivariate normal distribution
    proposal <- independent_proposal(
      proposal_mean = proposal_mean,
      proposal_chol = proposal_chol
    )

    # Log-subposterior of proposed beta
    proposal_log_subposterior <- log_subposterior(
      beta = proposal,
      y_subset = y_subset,
      X_subset = X_subset,
      prior_scale = prior_scale,
      num_subsets = num_subsets
    )

    # Proposal density of proposed beta
    proposal_log_proposal_density <- log_proposal_density(proposal)

    # Independent MH acceptance probability
    # Proposal-density correction is required because the proposal is not symmetric.
    log_acceptance_prob <-
      proposal_log_subposterior -
      current_log_subposterior +
      current_log_proposal_density -
      proposal_log_proposal_density

    # Accept or reject
    if (is.finite(log_acceptance_prob) &&
        log(runif(1)) < log_acceptance_prob) {
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


# Start runtime measurement
cmc_imh_start_time <- Sys.time()


# Safe core detection
available_cores <- detectCores()

if (is.na(available_cores)) {
  available_cores <- 2
}

num_cores <- max(
  1,
  min(cmc_num_shards, available_cores - 1)
)

cat(
  "\nRunning Parallel CMC-IMH shard chains across",
  num_cores,
  "cores...\n"
)


# Create cluster
cl <- makeCluster(num_cores)

# Run shard chains in parallel and always close the cluster afterward
parallel_imh_results <- tryCatch(
  {
    # Reproducible random numbers across parallel workers
    clusterSetRNGStream(cl, iseed = 4)

    # Export required objects and functions to worker processes
    clusterExport(
      cl,
      varlist = c(
        "X",
        "y",
        "data_used",
        "cmc_shard_id",
        "imh_subset_block",
        "log_subposterior",
        "log_prior",
        "log_likelihood",
        "independent_proposal",
        "mahalanobis_opt",
        "cmc_iterations",
        "cmc_burn_in",
        "prior_scale_value",
        "cmc_num_shards",
        "param_names",
        "cmc_imh_tuning_factor"
      ),
      envir = environment()
    )

    parLapply(
      cl,
      seq_len(cmc_num_shards),
      function(k) {
        subset_rows <- which(cmc_shard_id == k)

        y_subset <- y[subset_rows]
        X_subset <- X[subset_rows, , drop = FALSE]

        # Fit a logistic regression model on the current shard.
        # This provides a shard-specific proposal mean and covariance.
        subset_data <- data_used[subset_rows, , drop = FALSE]

        subset_logistic_model <- glm(
          accident_severity_binary ~ .,
          data = subset_data,
          family = binomial
        )

        # Shard-specific proposal mean
        subset_proposal_mean <- coef(subset_logistic_model)
        subset_proposal_mean[!is.finite(subset_proposal_mean)] <- 0

        # Shard-specific proposal covariance
        # No multiplication by cmc_num_shards is used here because
        # This proposal covariance is already estimated from one shard.
        subset_proposal_cov <-
          cmc_imh_tuning_factor^2 *
          vcov(subset_logistic_model)

        subset_proposal_cov <- as.matrix(subset_proposal_cov)

        # Cholesky decomposition is used to generate correlated multivariate normal proposals
        subset_proposal_chol <- chol(subset_proposal_cov)

        # Precompute shard-specific independent proposal log-density
        subset_log_proposal_density <- mahalanobis_opt(
          mean = subset_proposal_mean,
          cov_matrix = subset_proposal_cov
        )

        subset_result <- imh_subset_block(
          start_value = subset_proposal_mean,
          iterations = cmc_iterations,
          y_subset = y_subset,
          X_subset = X_subset,
          proposal_mean = subset_proposal_mean,
          proposal_chol = subset_proposal_chol,
          log_proposal_density = subset_log_proposal_density,
          prior_scale = prior_scale_value,
          num_subsets = cmc_num_shards,
          param_names = param_names
        )

        subset_chain <- subset_result$chain

        post_burn_in_chain <-
          subset_chain[-seq_len(cmc_burn_in + 1), , drop = FALSE]

        return(
          list(
            chain = post_burn_in_chain,
            acceptance_rate = subset_result$acceptance_rate
          )
        )
      }
    )
  },
  finally = {
    stopCluster(cl)
  }
)


# Extract shard chains and acceptance rates
cmc_imh_subset_samples_list <- lapply(
  parallel_imh_results,
  function(result) result$chain
)

cmc_imh_acceptance_rates <- sapply(
  parallel_imh_results,
  function(result) result$acceptance_rate
)

for (k in seq_len(cmc_num_shards)) {
  cat(
    "Shard", k,
    "Parallel CMC-IMH acceptance rate:",
    cmc_imh_acceptance_rates[k],
    "\n"
  )
}


# Combine Parallel CMC-IMH subposterior samples
cmc_imh_samples <- combine_consensus_samples(
  subposterior_samples_list = cmc_imh_subset_samples_list,
  param_names = param_names
)


# End runtime measurement
cmc_imh_end_time <- Sys.time()

cmc_imh_runtime <- as.numeric(
  difftime(cmc_imh_end_time, cmc_imh_start_time, units = "secs")
)

runtime_table <- rbind(
  runtime_table,
  data.frame(
    Method = "Parallel CMC-IMH",
    Runtime_Seconds = cmc_imh_runtime,
    Runtime_Minutes = cmc_imh_runtime / 60
  )
)

# Posterior summary for Parallel CMC-IMH
cmc_imh_post_stats_table <- posterior_statistics(cmc_imh_samples)

print(cmc_imh_post_stats_table)

# Density plots for Consensus Monte Carlo samples
# These are combined consensus samples, not one single Markov chain.
consensus_density_plot(cmc_imh_samples)



# 11. Compare all methods

cat("\nFull-data RWMH acceptance rate:\n")
print(rwmh_result$acceptance_rate)

cat("\nFull-data IMH acceptance rate:\n")
print(imh_result$acceptance_rate)

cat("\nParallel CMC-RWMH shard acceptance rates:\n")
print(cmc_rwmh_acceptance_rates)

cat("\nMean Parallel CMC-RWMH acceptance rate:\n")
print(mean(cmc_rwmh_acceptance_rates))

cat("\nParallel CMC-IMH shard acceptance rates:\n")
print(cmc_imh_acceptance_rates)

cat("\nMean Parallel CMC-IMH acceptance rate:\n")
print(mean(cmc_imh_acceptance_rates))


cat("\nFull-data RWMH ESS and MCSE:\n")
print(
  rwmh_post_stats_table[, c("Parameter", "ESS", "MCSE")]
)

cat("\nFull-data IMH ESS and MCSE:\n")
print(
  imh_post_stats_table[, c("Parameter", "ESS", "MCSE")]
)

cat("\nParallel CMC-RWMH ESS and MCSE:\n")
print(
  cmc_rwmh_post_stats_table[, c("Parameter", "ESS", "MCSE")]
)

cat("\nParallel CMC-IMH ESS and MCSE:\n")
print(
  cmc_imh_post_stats_table[, c("Parameter", "ESS", "MCSE")]
)


cat("\nPosterior Summary: Full-data RWMH\n")
print(rwmh_post_stats_table)

cat("\nPosterior Summary: Full-data IMH\n")
print(imh_post_stats_table)

cat("\nPosterior Summary: Parallel CMC-RWMH\n")
print(cmc_rwmh_post_stats_table)

cat("\nPosterior Summary: Parallel CMC-IMH\n")
print(cmc_imh_post_stats_table)


cat("\nRuntime Comparison:\n")
print(runtime_table)
