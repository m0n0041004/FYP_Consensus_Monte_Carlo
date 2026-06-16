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

# Estimated coefficient covariance
# Used later as a reference covariance matrix for MH proposal distributions.
cov_beta <- vcov(logistic_model)


# 3. Prepare response and design matrix

# Recode response variable
# serious_fatal is coded as 1 because it is the event being modeled
y <- ifelse(data_used$accident_severity_binary == "serious_fatal", 1, 0)

# Extract design matrix from the frequentist logistic regression model
X <- model.matrix(logistic_model)

# Store parameter names
param_names <- colnames(X)


# 4. Define prior, likelihood, and posterior

# Log-prior
# Each coefficient is assigned a Cauchy(0, 2.5) prior.
log_prior <- function(beta, prior_scale = 2.5) {
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
log_posterior <- function(beta, y, X, prior_scale = 2.5) {
  log_prior(beta, prior_scale = prior_scale) +
    log_likelihood(beta, y, X)
}


# 5. General posterior summary function

posterior_statistics <- function(post_samples, alpha = 0.05, ess = NULL) {
  if (is.null(ess)) {
    post_ess <- coda::effectiveSize(post_samples)
  } else {
    post_ess <- ess
  }

  # Posterior summaries on log-odds scale
  post_mean <- colMeans(post_samples)
  post_median <- apply(post_samples, 2, median)
  post_sd <- apply(post_samples, 2, sd)

  # Monte Carlo standard error
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
      "(",
      round(lower_cri, 4),
      ", ",
      round(upper_cri, 4),
      ")"
    ),
    Odds_Ratio = exp(post_mean),
    row.names = NULL
  )

  return(stats_df)
}

gelman_psrf_table <- function(gelman_object, method_name) {
  psrf <- as.data.frame(gelman_object$psrf)
  psrf$Parameter <- rownames(psrf)
  rownames(psrf) <- NULL

  psrf <- psrf[, c("Parameter", "Point est.", "Upper C.I.")]
  names(psrf) <- c("Parameter", "Rhat", "Rhat_Upper_CI")
  psrf$Method <- method_name

  psrf[, c("Method", "Parameter", "Rhat", "Rhat_Upper_CI")]
}


# 6. Functions

hist_trace_plot <- function(samples,
                            param_names = colnames(samples),
                            params_per_page = 2) {
  # Save current plotting settings
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

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

  num_params <- length(param_names)

  for (start in seq(1, num_params, by = params_per_page)) {
    end <- min(start + params_per_page - 1, num_params)
    index <- start:end

    par(mfrow = c(1, length(index)))

    for (i in index) {
      acf(
        samples[, i],
        main = paste("Correlogram of", param_names[i]),
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

# Store runtimes
runtime_table <- data.frame(
  Method = character(),
  Runtime_Seconds = numeric(),
  Runtime_Minutes = numeric()
)

library(parallel)


# 7. Number of iterations and burn-in for MCMC chains

num_iterations <- 100000
burn_in_fraction <- 0.2
burn_in <- round(num_iterations * burn_in_fraction)
num_chains <- 4


# 8. Full-data RWMH

set.seed(1)

# Number of parameters
d <- length(coef(logistic_model))

# Starting value from frequentist logistic regression
start_value <- coef(logistic_model)

# Tuning factor for random-walk proposal
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

full_data_rwmh <- function(start_value,
                           iterations,
                           y,
                           X,
                           proposal_chol,
                           burn_in = 0,
                           prior_scale = 2.5,
                           param_names = NULL) {

  num_parameters <- length(start_value)

  # Create matrix to store MCMC samples
  chain <- matrix(
    NA_real_,
    nrow = iterations + 1,
    ncol = num_parameters
  )

  if (!is.null(param_names)) {
    colnames(chain) <- param_names
  }

  # Store starting value
  chain[1, ] <- start_value

  # Store current state
  current <- start_value

  current_log_posterior <- log_posterior(
    beta = current,
    y = y,
    X = X,
    prior_scale = prior_scale
  )

  # Track only post-burn-in acceptances
  post_burn_in_acceptances <- 0L

  for (i in seq_len(iterations)) {
    # Generate proposed beta from random-walk proposal distribution
    proposal <- random_walk_proposal(
      current_beta = current,
      proposal_chol = proposal_chol
    )

    proposal_log_posterior <- log_posterior(
      beta = proposal,
      y = y,
      X = X,
      prior_scale = prior_scale
    )

    # Random-walk MH log acceptance probability
    log_acceptance_prob <-
      proposal_log_posterior -
      current_log_posterior

    # Accept or reject proposal
    if (!is.na(log_acceptance_prob) &&
        log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_posterior <- proposal_log_posterior

      if (i > burn_in) {
        post_burn_in_acceptances <- post_burn_in_acceptances + 1L
      }
    }

    chain[i + 1, ] <- current
  }

  post_burn_in_acceptance_rate <-
    post_burn_in_acceptances / (iterations - burn_in)

  cat(
    "Full-data RWMH post-burn-in acceptance rate:",
    post_burn_in_acceptance_rate,
    "\n"
  )

  return(
    list(
      chain = chain,
      post_burn_in_acceptance_rate = post_burn_in_acceptance_rate
    )
  )
}

# Start runtime measurement
rwmh_start_time <- Sys.time()

# Create cluster for independent full-data chains
cl <- makeCluster(num_chains)

# Reproducible random numbers across parallel chains
clusterSetRNGStream(cl, iseed = 100)

# Export required objects and functions to parallel workers
clusterExport(
  cl,
  varlist = c(
    "full_data_rwmh",
    "start_value",
    "num_iterations",
    "y",
    "X",
    "proposal_chol_rwmh",
    "burn_in",
    "param_names",
    "log_posterior",
    "log_prior",
    "log_likelihood",
    "random_walk_proposal"
  ),
  envir = environment()
)

# Run four independent Full-data RWMH chains in parallel
full_data_rwmh_results <- parLapply(
  cl,
  seq_len(num_chains),
  function(chain) {
    full_data_rwmh(
      start_value = start_value,
      iterations = num_iterations,
      y = y,
      X = X,
      proposal_chol = proposal_chol_rwmh,
      burn_in = burn_in,
      prior_scale = 2.5,
      param_names = param_names
    )
  }
)

# Close cluster
stopCluster(cl)

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

# Extract post-burn-in samples from each Full-data RWMH chain
full_data_rwmh_post_samples_list <- lapply(
  full_data_rwmh_results,
  function(result) {
    result$chain[-seq_len(burn_in + 1), , drop = FALSE]
  }
)

# Chain-specific post-burn-in acceptance rates
full_data_rwmh_post_burn_in_acceptance_rates <- sapply(
  full_data_rwmh_results,
  function(result) result$post_burn_in_acceptance_rate
)

# Gelman-Rubin diagnostics across independent full-data chains
full_data_rwmh_mcmc_list <- coda::mcmc.list(
  lapply(full_data_rwmh_post_samples_list, coda::mcmc)
)

full_data_rwmh_gelman_diag <- coda::gelman.diag(
  full_data_rwmh_mcmc_list,
  autoburnin = FALSE
)

full_data_rwmh_ess_by_parameter <- coda::effectiveSize(
  full_data_rwmh_mcmc_list
)

full_data_rwmh_ess_table <- data.frame(
  Method = "Full-data RWMH",
  Parameter = names(full_data_rwmh_ess_by_parameter),
  ESS = as.numeric(full_data_rwmh_ess_by_parameter),
  row.names = NULL
)

cat("\nFull-data RWMH chain post-burn-in acceptance rates:\n")
print(full_data_rwmh_post_burn_in_acceptance_rates)

cat("\nMean Full-data RWMH post-burn-in acceptance rate:\n")
print(mean(full_data_rwmh_post_burn_in_acceptance_rates))

cat("\nFull-data RWMH Gelman-Rubin diagnostics:\n")
print(gelman_psrf_table(full_data_rwmh_gelman_diag, "Full-data RWMH"),
      digits = 8)

cat("\nFull-data RWMH ESS by parameter from coda::mcmc.list:\n")
print(full_data_rwmh_ess_table)

# The four full-data chains are kept separate in coda::mcmc.list
# for Gelman-Rubin diagnostics and ESS calculation.
# Pooled post-burn-in samples are used only for posterior distribution
# summaries such as mean, median, posterior SD, credible interval,
# and odds ratio.
# MCSE uses posterior SD from the pooled draws and ESS from the
# coda::mcmc.list object.
full_data_rwmh_pooled_post_samples <- do.call(
  rbind,
  full_data_rwmh_post_samples_list
)

# This object contains pooled post-burn-in samples and should not be used
# for autocorrelation-based diagnostics such as ESS or ACF.
full_data_rwmh_post_samples <- full_data_rwmh_pooled_post_samples

# Posterior summary table
full_data_rwmh_post_stats_table <- posterior_statistics(
  post_samples = full_data_rwmh_pooled_post_samples,
  ess = full_data_rwmh_ess_by_parameter
)

cat(
  "\nFull-data RWMH posterior summary using pooled post-burn-in draws",
  "and mcmc.list ESS:\n"
)
print(full_data_rwmh_post_stats_table)

# Formal diagnostics use all four chains through coda::mcmc.list.
# For visual diagnostics, one representative post-burn-in chain is shown.
# This avoids plotting trace or ACF from a pooled artificial chain.
full_data_rwmh_plot_chain_id <- 1

cat(
  "\nFull-data RWMH histogram, trace, and ACF plots",
  "shown for representative chain",
  full_data_rwmh_plot_chain_id,
  "only.\n"
)

hist_trace_plot(
  full_data_rwmh_post_samples_list[[full_data_rwmh_plot_chain_id]]
)

acf_mcmc_plot(
  full_data_rwmh_post_samples_list[[full_data_rwmh_plot_chain_id]]
)

# Print runtime so far
print(runtime_table)


# 9. Full-data IMH

set.seed(2)

# Proposal mean from frequentist logistic regression
proposal_mean_imh <- coef(logistic_model)

# Tuning factor for independent proposal
imh_tuning_factor <- 1.5

# Independent proposal covariance
proposal_cov_imh <- imh_tuning_factor^2 * vcov(logistic_model)
proposal_cov_imh <- as.matrix(proposal_cov_imh)

# Cholesky decomposition for proposal covariance
proposal_chol_imh <- chol(proposal_cov_imh)

# Precompute inverse proposal covariance for efficient log-density calculation
proposal_cov_inv_imh <- chol2inv(proposal_chol_imh)

# Independent proposal function
independent_proposal <- function(proposal_mean, proposal_chol) {
  as.vector(
    proposal_mean + t(proposal_chol) %*% rnorm(length(proposal_mean))
  )
}

# Fixed independent proposal log-density
# The normalizing constant is omitted because it cancels in the MH ratio.
log_proposal_density_imh <- function(beta) {
  as.numeric(
    -0.5 * mahalanobis(
      x = beta,
      center = proposal_mean_imh,
      cov = proposal_cov_inv_imh,
      inverted = TRUE
    )
  )
}

full_data_imh <- function(start_value,
                          iterations,
                          y,
                          X,
                          proposal_mean,
                          proposal_chol,
                          log_proposal_density,
                          burn_in = 0,
                          prior_scale = 2.5,
                          param_names = NULL) {

  num_parameters <- length(start_value)

  # Create matrix to store MCMC samples
  chain <- matrix(
    NA_real_,
    nrow = iterations + 1,
    ncol = num_parameters
  )

  if (!is.null(param_names)) {
    colnames(chain) <- param_names
  }

  # Store starting value
  chain[1, ] <- start_value

  # Store current state
  current <- start_value

  current_log_posterior <- log_posterior(
    beta = current,
    y = y,
    X = X,
    prior_scale = prior_scale
  )

  current_log_proposal_density <- log_proposal_density(current)

  # Track only post-burn-in acceptances
  post_burn_in_acceptances <- 0L

  for (i in seq_len(iterations)) {
    # Generate proposed beta from fixed proposal distribution
    proposal <- independent_proposal(
      proposal_mean = proposal_mean,
      proposal_chol = proposal_chol
    )

    proposal_log_posterior <- log_posterior(
      beta = proposal,
      y = y,
      X = X,
      prior_scale = prior_scale
    )

    proposal_log_proposal_density <- log_proposal_density(proposal)

    # Independent MH log acceptance probability
    log_acceptance_prob <-
      proposal_log_posterior -
      current_log_posterior +
      current_log_proposal_density -
      proposal_log_proposal_density

    # Accept or reject proposal
    if (!is.na(log_acceptance_prob) &&
        log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_posterior <- proposal_log_posterior
      current_log_proposal_density <- proposal_log_proposal_density

      if (i > burn_in) {
        post_burn_in_acceptances <- post_burn_in_acceptances + 1L
      }
    }

    chain[i + 1, ] <- current
  }

  post_burn_in_acceptance_rate <-
    post_burn_in_acceptances / (iterations - burn_in)

  cat(
    "Full-data IMH post-burn-in acceptance rate:",
    post_burn_in_acceptance_rate,
    "\n"
  )

  return(
    list(
      chain = chain,
      post_burn_in_acceptance_rate = post_burn_in_acceptance_rate
    )
  )
}

# Start runtime measurement
imh_start_time <- Sys.time()

# Create cluster for independent full-data chains
cl <- makeCluster(num_chains)

# Reproducible random numbers across parallel chains
clusterSetRNGStream(cl, iseed = 200)

# Export required objects and functions to parallel workers
clusterExport(
  cl,
  varlist = c(
    "full_data_imh",
    "start_value",
    "num_iterations",
    "y",
    "X",
    "proposal_mean_imh",
    "proposal_chol_imh",
    "log_proposal_density_imh",
    "burn_in",
    "param_names",
    "log_posterior",
    "log_prior",
    "log_likelihood",
    "independent_proposal",
    "proposal_cov_inv_imh"
  ),
  envir = environment()
)

# Run four independent Full-data IMH chains in parallel
full_data_imh_results <- parLapply(
  cl,
  seq_len(num_chains),
  function(chain) {
    full_data_imh(
      start_value = start_value,
      iterations = num_iterations,
      y = y,
      X = X,
      proposal_mean = proposal_mean_imh,
      proposal_chol = proposal_chol_imh,
      log_proposal_density = log_proposal_density_imh,
      burn_in = burn_in,
      prior_scale = 2.5,
      param_names = param_names
    )
  }
)

# Close cluster
stopCluster(cl)

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

# Extract post-burn-in samples from each Full-data IMH chain
full_data_imh_post_samples_list <- lapply(
  full_data_imh_results,
  function(result) {
    result$chain[-seq_len(burn_in + 1), , drop = FALSE]
  }
)

# Chain-specific post-burn-in acceptance rates
full_data_imh_post_burn_in_acceptance_rates <- sapply(
  full_data_imh_results,
  function(result) result$post_burn_in_acceptance_rate
)

# Gelman-Rubin diagnostics across independent full-data chains
full_data_imh_mcmc_list <- coda::mcmc.list(
  lapply(full_data_imh_post_samples_list, coda::mcmc)
)

full_data_imh_gelman_diag <- coda::gelman.diag(
  full_data_imh_mcmc_list,
  autoburnin = FALSE
)

full_data_imh_ess_by_parameter <- coda::effectiveSize(
  full_data_imh_mcmc_list
)

full_data_imh_ess_table <- data.frame(
  Method = "Full-data IMH",
  Parameter = names(full_data_imh_ess_by_parameter),
  ESS = as.numeric(full_data_imh_ess_by_parameter),
  row.names = NULL
)

cat("\nFull-data IMH chain post-burn-in acceptance rates:\n")
print(full_data_imh_post_burn_in_acceptance_rates)

cat("\nMean Full-data IMH post-burn-in acceptance rate:\n")
print(mean(full_data_imh_post_burn_in_acceptance_rates))

cat("\nFull-data IMH Gelman-Rubin diagnostics:\n")
print(gelman_psrf_table(full_data_imh_gelman_diag, "Full-data IMH"),
      digits = 8)

cat("\nFull-data IMH ESS by parameter from coda::mcmc.list:\n")
print(full_data_imh_ess_table)

# The four full-data chains are kept separate in coda::mcmc.list
# for Gelman-Rubin diagnostics and ESS calculation.
# Pooled post-burn-in samples are used only for posterior distribution
# summaries such as mean, median, posterior SD, credible interval,
# and odds ratio.
# MCSE uses posterior SD from the pooled draws and ESS from the
# coda::mcmc.list object.
full_data_imh_pooled_post_samples <- do.call(
  rbind,
  full_data_imh_post_samples_list
)

# This object contains pooled post-burn-in samples and should not be used
# for autocorrelation-based diagnostics such as ESS or ACF.
full_data_imh_post_samples <- full_data_imh_pooled_post_samples

# Posterior summary table
full_data_imh_post_stats_table <- posterior_statistics(
  post_samples = full_data_imh_pooled_post_samples,
  ess = full_data_imh_ess_by_parameter
)

cat(
  "\nFull-data IMH posterior summary using pooled post-burn-in draws",
  "and mcmc.list ESS:\n"
)
print(full_data_imh_post_stats_table)

# Formal diagnostics use all four chains through coda::mcmc.list.
# For visual diagnostics, one representative post-burn-in chain is shown.
# This avoids plotting trace or ACF from a pooled artificial chain.
full_data_imh_plot_chain_id <- 1

cat(
  "\nFull-data IMH histogram, trace, and ACF plots",
  "shown for representative chain",
  full_data_imh_plot_chain_id,
  "only.\n"
)

hist_trace_plot(
  full_data_imh_post_samples_list[[full_data_imh_plot_chain_id]]
)

acf_mcmc_plot(
  full_data_imh_post_samples_list[[full_data_imh_plot_chain_id]]
)

# Print runtime comparison so far
print(runtime_table)

# 10. CMC-RWMH

set.seed(3)

# Number of shards
# K = 10 is used to divide the computation into ten shards.
cmc_num_shards <- 10

# Number of MCMC iterations for each shard chain
# Use the same number of iterations as the full-data MCMC chains for comparability.
cmc_iterations <- num_iterations
cmc_burn_in <- round(cmc_iterations * burn_in_fraction)

# Stratified split by response class
# Observations within each response class are assigned across the K shards.
cmc_shard_id <- integer(length(y))

for (response_class in sort(unique(y))) {
  response_rows <- which(y == response_class)
  shuffled_response_rows <- sample(response_rows)

  response_shard_id <- rep(
    seq_len(cmc_num_shards),
    length.out = length(shuffled_response_rows)
  )

  cmc_shard_id[shuffled_response_rows] <- sample(response_shard_id)
}

cmc_shard_sizes <- table(cmc_shard_id)
cmc_shard_response_counts <- table(cmc_shard_id, y)

# Check shard balance
cmc_shard_sizes

# Check response distribution within each shard
cmc_shard_response_counts

# Check predictor distributions within each shard
table(cmc_shard_id, data_used$road_type)
table(cmc_shard_id, data_used$speed_limit)

# Subposterior log-density
# Each shard uses 1 / K of the prior so that multiplying all
# subposteriors gives the full posterior structure.
log_subposterior <- function(beta,
                             y_shard,
                             X_shard,
                             prior_scale,
                             num_shards) {
  (1 / num_shards) * log_prior(beta, prior_scale = prior_scale) +
    log_likelihood(beta, y_shard, X_shard)
}

# RWMH sampler for one shard
cmc_rwmh_shard <- function(start_value,
                           iterations,
                           y_shard,
                           X_shard,
                           proposal_chol,
                           burn_in = 0,
                           prior_scale = 2.5,
                           num_shards,
                           param_names = NULL) {

  num_parameters <- length(start_value)

  chain <- matrix(
    NA_real_,
    nrow = iterations + 1,
    ncol = num_parameters
  )

  if (!is.null(param_names)) {
    colnames(chain) <- param_names
  }

  chain[1, ] <- start_value

  current <- start_value

  current_log_subposterior <- log_subposterior(
    beta = current,
    y_shard = y_shard,
    X_shard = X_shard,
    prior_scale = prior_scale,
    num_shards = num_shards
  )

  # Track only post-burn-in acceptances
  post_burn_in_acceptances <- 0L

  for (i in seq_len(iterations)) {
    proposal <- random_walk_proposal(
      current_beta = current,
      proposal_chol = proposal_chol
    )

    proposal_log_subposterior <- log_subposterior(
      beta = proposal,
      y_shard = y_shard,
      X_shard = X_shard,
      prior_scale = prior_scale,
      num_shards = num_shards
    )

    # Random-walk MH log acceptance probability
    log_acceptance_prob <-
      proposal_log_subposterior -
      current_log_subposterior

    if (!is.na(log_acceptance_prob) &&
        log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_subposterior <- proposal_log_subposterior

      if (i > burn_in) {
        post_burn_in_acceptances <- post_burn_in_acceptances + 1L
      }
    }

    chain[i + 1, ] <- current
  }

  post_burn_in_acceptance_rate <-
    post_burn_in_acceptances / (iterations - burn_in)

  return(
    list(
      chain = chain,
      post_burn_in_acceptance_rate = post_burn_in_acceptance_rate
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

# Create cluster
num_cores <- cmc_num_shards
cl <- makeCluster(num_cores)

# Reproducible random numbers across parallel workers
clusterSetRNGStream(cl, iseed = 3)

# Export required objects and functions to parallel workers
clusterExport(
  cl,
  varlist = c(
    "X",
    "y",
    "cmc_shard_id",
    "cmc_rwmh_shard",
    "log_subposterior",
    "log_prior",
    "log_likelihood",
    "random_walk_proposal",
    "cmc_rwmh_proposal_chol",
    "cmc_iterations",
    "cmc_burn_in",
    "cmc_num_shards",
    "num_chains",
    "start_value",
    "param_names"
  ),
  envir = environment()
)

# Run shard workers in parallel; each shard runs four chains sequentially
cmc_rwmh_results <- parLapply(
  cl,
  seq_len(cmc_num_shards),
  function(k) {
    shard_rows <- which(cmc_shard_id == k)

    chain_results <- lapply(
      seq_len(num_chains),
      function(chain) {
        cmc_rwmh_shard(
          start_value = start_value,
          iterations = cmc_iterations,
          y_shard = y[shard_rows],
          X_shard = X[shard_rows, , drop = FALSE],
          proposal_chol = cmc_rwmh_proposal_chol,
          burn_in = cmc_burn_in,
          prior_scale = 2.5,
          num_shards = cmc_num_shards,
          param_names = param_names
        )
      }
    )

    post_samples_list <- lapply(
      chain_results,
      function(result) {
        shard_chain <- result$chain

        post_burn_in_chain <-
          shard_chain[-seq_len(cmc_burn_in + 1), , drop = FALSE]

        post_burn_in_chain
      }
    )

    post_burn_in_acceptance_rates <- sapply(
      chain_results,
      function(result) result$post_burn_in_acceptance_rate
    )

    mcmc_list <- coda::mcmc.list(
      lapply(post_samples_list, coda::mcmc)
    )

    gelman_diag <- coda::gelman.diag(
      mcmc_list,
      autoburnin = FALSE
    )

    ess_by_parameter <- coda::effectiveSize(mcmc_list)

    combined_post_samples <- do.call(
      rbind,
      post_samples_list
    )

    return(
      list(
        post_samples_list = post_samples_list,
        pooled_post_samples = combined_post_samples,
        post_burn_in_acceptance_rates = post_burn_in_acceptance_rates,
        gelman_diag = gelman_diag,
        ess_by_parameter = ess_by_parameter
      )
    )
  }
)

# Close cluster
stopCluster(cl)

# Extract shard chains and post-burn-in acceptance rates
cmc_rwmh_shard_post_samples_by_chain_list <- lapply(
  cmc_rwmh_results,
  function(result) result$post_samples_list
)

# Kept for backward compatibility and optional shard-level summaries.
cmc_rwmh_subposterior_samples_list <- lapply(
  cmc_rwmh_results,
  function(result) result$pooled_post_samples
)

cmc_rwmh_post_burn_in_acceptance_rates_by_shard <- do.call(
  rbind,
  lapply(
    cmc_rwmh_results,
    function(result) result$post_burn_in_acceptance_rates
  )
)

rownames(cmc_rwmh_post_burn_in_acceptance_rates_by_shard) <-
  paste0("Shard_", seq_len(cmc_num_shards))

colnames(cmc_rwmh_post_burn_in_acceptance_rates_by_shard) <-
  paste0("Chain_", seq_len(num_chains))

cmc_rwmh_mean_post_burn_in_acceptance_rates <- rowMeans(
  cmc_rwmh_post_burn_in_acceptance_rates_by_shard
)

cmc_rwmh_gelman_diag_list <- lapply(
  cmc_rwmh_results,
  function(result) result$gelman_diag
)

cmc_rwmh_shard_ess_list <- lapply(
  cmc_rwmh_results,
  function(result) result$ess_by_parameter
)

cmc_rwmh_shard_ess_table <- do.call(
  rbind,
  lapply(
    seq_along(cmc_rwmh_shard_ess_list),
    function(k) {
      ess_k <- cmc_rwmh_shard_ess_list[[k]]

      data.frame(
        Method = "CMC-RWMH",
        Shard = k,
        Parameter = names(ess_k),
        ESS = as.numeric(ess_k),
        row.names = NULL
      )
    }
  )
)

for (k in seq_len(cmc_num_shards)) {
  cat(
    "Shard", k,
    "CMC-RWMH mean post-burn-in acceptance rate:",
    cmc_rwmh_mean_post_burn_in_acceptance_rates[k],
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

  # Consensus samples
  consensus_samples <- t(consensus_cov %*% weighted_sum)

  colnames(consensus_samples) <- param_names

  return(consensus_samples)
}

# For CMC, chain identity is preserved through the consensus step.
# Final CMC chain c is obtained by combining shard chain c from all shards.
# The four final consensus chains are used as a coda::mcmc.list for
# Gelman-Rubin diagnostics and ESS calculation.
# The pooled final consensus sample matrix is used only for posterior
# summaries, RMSE, and density plots.
cmc_rwmh_consensus_samples_list <- lapply(
  seq_len(num_chains),
  function(chain_id) {
    shard_samples_for_chain <- lapply(
      cmc_rwmh_results,
      function(result) {
        result$post_samples_list[[chain_id]]
      }
    )

    combine_consensus_samples(
      subposterior_samples_list = shard_samples_for_chain,
      param_names = param_names
    )
  }
)

cmc_rwmh_consensus_mcmc_list <- coda::mcmc.list(
  lapply(cmc_rwmh_consensus_samples_list, coda::mcmc)
)

cmc_rwmh_consensus_gelman_diag <- coda::gelman.diag(
  cmc_rwmh_consensus_mcmc_list,
  autoburnin = FALSE
)

cmc_rwmh_ess_by_parameter <- coda::effectiveSize(
  cmc_rwmh_consensus_mcmc_list
)

cmc_rwmh_ess_table <- data.frame(
  Method = "CMC-RWMH",
  Parameter = names(cmc_rwmh_ess_by_parameter),
  ESS = as.numeric(cmc_rwmh_ess_by_parameter),
  row.names = NULL
)

# The final CMC-RWMH posterior sample matrix is pooled from four
# final consensus chains. It is used for posterior summaries only.
# ESS and Rhat are computed from cmc_rwmh_consensus_mcmc_list.
cmc_rwmh_samples <- do.call(
  rbind,
  cmc_rwmh_consensus_samples_list
)

# End runtime measurement
cmc_rwmh_end_time <- Sys.time()

cmc_rwmh_runtime <- as.numeric(
  difftime(cmc_rwmh_end_time, cmc_rwmh_start_time, units = "secs")
)

runtime_table <- rbind(
  runtime_table,
  data.frame(
    Method = "CMC-RWMH",
    Runtime_Seconds = cmc_rwmh_runtime,
    Runtime_Minutes = cmc_rwmh_runtime / 60
  )
)

# Posterior summary for CMC-RWMH
cmc_rwmh_post_stats_table <- posterior_statistics(
  post_samples = cmc_rwmh_samples,
  ess = cmc_rwmh_ess_by_parameter
)

cat("\nCMC-RWMH final consensus Gelman-Rubin diagnostics:\n")
print(
  gelman_psrf_table(
    cmc_rwmh_consensus_gelman_diag,
    "CMC-RWMH"
  ),
  digits = 8
)

cat("\nCMC-RWMH final consensus ESS by parameter:\n")
print(cmc_rwmh_ess_table)

cat("\nCMC-RWMH shard-level ESS by parameter:\n")
print(cmc_rwmh_shard_ess_table)

print(cmc_rwmh_post_stats_table)

# RMSE of posterior mean
# The Full-data RWMH posterior mean is used as the reference.
full_data_rwmh_posterior_mean <- colMeans(full_data_rwmh_post_samples)
cmc_rwmh_posterior_mean <- colMeans(cmc_rwmh_samples)

cmc_rwmh_posterior_mean_error <-
  cmc_rwmh_posterior_mean - full_data_rwmh_posterior_mean

cmc_rwmh_posterior_mean_rmse <- sqrt(
  mean(cmc_rwmh_posterior_mean_error^2)
)

cmc_rwmh_rmse_table <- data.frame(
  Method = "CMC-RWMH",
  Reference = "Full-data RWMH",
  Posterior_Mean_RMSE = cmc_rwmh_posterior_mean_rmse
)

print(cmc_rwmh_rmse_table)

# Parameter-wise posterior mean error
cmc_rwmh_posterior_mean_error_table <- data.frame(
  Parameter = param_names,
  Full_Data_RWMH_Mean = full_data_rwmh_posterior_mean,
  CMC_RWMH_Mean = cmc_rwmh_posterior_mean,
  Error = cmc_rwmh_posterior_mean_error,
  Squared_Error = cmc_rwmh_posterior_mean_error^2,
  row.names = NULL
)

print(cmc_rwmh_posterior_mean_error_table)

# Density plots for consensus samples
# These samples are produced by combining shard subposterior samples,
# not by running one single Markov chain.
consensus_density_plot(cmc_rwmh_samples)

# Trace and ACF plots use one representative final consensus chain.
# This avoids plotting time-series diagnostics from pooled samples.
cmc_rwmh_plot_chain_id <- 1

hist_trace_plot(
  cmc_rwmh_consensus_samples_list[[cmc_rwmh_plot_chain_id]]
)

acf_mcmc_plot(
  cmc_rwmh_consensus_samples_list[[cmc_rwmh_plot_chain_id]]
)


# 11. CMC-IMH

set.seed(4)

# Tuning factor for shard-specific IMH proposal
# A value of 1.5 is used to match the Full-data IMH tuning factor.
cmc_imh_tuning_factor <- 1.5

# IMH sampler for one shard
cmc_imh_shard <- function(start_value,
                          iterations,
                          y_shard,
                          X_shard,
                          proposal_mean,
                          proposal_chol,
                          log_proposal_density,
                          burn_in = 0,
                          prior_scale = 2.5,
                          num_shards,
                          param_names = NULL) {

  num_parameters <- length(start_value)

  # Create matrix to store MCMC samples
  chain <- matrix(
    NA_real_,
    nrow = iterations + 1,
    ncol = num_parameters
  )

  if (!is.null(param_names)) {
    colnames(chain) <- param_names
  }

  # Store starting value
  chain[1, ] <- start_value

  # Store current state
  current <- start_value

  current_log_subposterior <- log_subposterior(
    beta = current,
    y_shard = y_shard,
    X_shard = X_shard,
    prior_scale = prior_scale,
    num_shards = num_shards
  )

  current_log_proposal_density <- log_proposal_density(current)

  # Track only post-burn-in acceptances
  post_burn_in_acceptances <- 0L

  for (i in seq_len(iterations)) {
    # Generate proposed beta from shard-specific independent proposal
    proposal <- independent_proposal(
      proposal_mean = proposal_mean,
      proposal_chol = proposal_chol
    )

    proposal_log_subposterior <- log_subposterior(
      beta = proposal,
      y_shard = y_shard,
      X_shard = X_shard,
      prior_scale = prior_scale,
      num_shards = num_shards
    )

    proposal_log_proposal_density <- log_proposal_density(proposal)

    # Independent MH log acceptance probability
    log_acceptance_prob <-
      proposal_log_subposterior -
      current_log_subposterior +
      current_log_proposal_density -
      proposal_log_proposal_density

    # Accept or reject proposal
    if (!is.na(log_acceptance_prob) &&
        log(runif(1)) < log_acceptance_prob) {
      current <- proposal
      current_log_subposterior <- proposal_log_subposterior
      current_log_proposal_density <- proposal_log_proposal_density

      if (i > burn_in) {
        post_burn_in_acceptances <- post_burn_in_acceptances + 1L
      }
    }

    chain[i + 1, ] <- current
  }

  post_burn_in_acceptance_rate <-
    post_burn_in_acceptances / (iterations - burn_in)

  return(
    list(
      chain = chain,
      post_burn_in_acceptance_rate = post_burn_in_acceptance_rate
    )
  )
}

# Start runtime measurement
cmc_imh_start_time <- Sys.time()

# Create cluster
num_cores <- cmc_num_shards
cl <- makeCluster(num_cores)

# Reproducible random numbers across parallel workers
clusterSetRNGStream(cl, iseed = 4)

# Export required objects and functions to parallel workers
clusterExport(
  cl,
  varlist = c(
    "X",
    "y",
    "data_used",
    "cmc_shard_id",
    "cmc_imh_shard",
    "log_subposterior",
    "log_prior",
    "log_likelihood",
    "independent_proposal",
    "cmc_iterations",
    "cmc_burn_in",
    "cmc_num_shards",
    "num_chains",
    "param_names",
    "cmc_imh_tuning_factor"
  ),
  envir = environment()
)

# Run shard workers in parallel; each shard runs four chains sequentially
cmc_imh_results <- parLapply(
  cl,
  seq_len(cmc_num_shards),
  function(k) {
    shard_rows <- which(cmc_shard_id == k)

    y_shard <- y[shard_rows]
    X_shard <- X[shard_rows, , drop = FALSE]
    shard_data <- data_used[shard_rows, , drop = FALSE]

    # Fit logistic regression model on the current shard
    shard_logistic_model <- glm(
      accident_severity_binary ~ .,
      data = shard_data,
      family = binomial
    )

    # Shard-specific proposal mean
    shard_proposal_mean <- coef(shard_logistic_model)

    # Shard-specific proposal covariance
    shard_proposal_cov <-
      cmc_imh_tuning_factor^2 *
      vcov(shard_logistic_model)

    shard_proposal_cov <- as.matrix(shard_proposal_cov)

    # Cholesky decomposition for shard-specific proposal covariance
    shard_proposal_chol <- chol(shard_proposal_cov)

    # Precompute inverse proposal covariance
    shard_proposal_cov_inv <- chol2inv(shard_proposal_chol)

    # Shard-specific independent proposal log-density
    shard_log_proposal_density <- function(beta) {
      as.numeric(
        -0.5 * mahalanobis(
          x = beta,
          center = shard_proposal_mean,
          cov = shard_proposal_cov_inv,
          inverted = TRUE
        )
      )
    }

    chain_results <- lapply(
      seq_len(num_chains),
      function(chain) {
        cmc_imh_shard(
          start_value = shard_proposal_mean,
          iterations = cmc_iterations,
          y_shard = y_shard,
          X_shard = X_shard,
          proposal_mean = shard_proposal_mean,
          proposal_chol = shard_proposal_chol,
          log_proposal_density = shard_log_proposal_density,
          burn_in = cmc_burn_in,
          prior_scale = 2.5,
          num_shards = cmc_num_shards,
          param_names = param_names
        )
      }
    )

    post_samples_list <- lapply(
      chain_results,
      function(result) {
        shard_chain <- result$chain

        post_burn_in_chain <-
          shard_chain[-seq_len(cmc_burn_in + 1), , drop = FALSE]

        post_burn_in_chain
      }
    )

    post_burn_in_acceptance_rates <- sapply(
      chain_results,
      function(result) result$post_burn_in_acceptance_rate
    )

    mcmc_list <- coda::mcmc.list(
      lapply(post_samples_list, coda::mcmc)
    )

    gelman_diag <- coda::gelman.diag(
      mcmc_list,
      autoburnin = FALSE
    )

    ess_by_parameter <- coda::effectiveSize(mcmc_list)

    combined_post_samples <- do.call(
      rbind,
      post_samples_list
    )

    return(
      list(
        post_samples_list = post_samples_list,
        pooled_post_samples = combined_post_samples,
        post_burn_in_acceptance_rates = post_burn_in_acceptance_rates,
        gelman_diag = gelman_diag,
        ess_by_parameter = ess_by_parameter
      )
    )
  }
)

# Close cluster
stopCluster(cl)

# Extract shard chains and post-burn-in acceptance rates
cmc_imh_shard_post_samples_by_chain_list <- lapply(
  cmc_imh_results,
  function(result) result$post_samples_list
)

# Kept for backward compatibility and optional shard-level summaries.
cmc_imh_subposterior_samples_list <- lapply(
  cmc_imh_results,
  function(result) result$pooled_post_samples
)

cmc_imh_post_burn_in_acceptance_rates_by_shard <- do.call(
  rbind,
  lapply(
    cmc_imh_results,
    function(result) result$post_burn_in_acceptance_rates
  )
)

rownames(cmc_imh_post_burn_in_acceptance_rates_by_shard) <-
  paste0("Shard_", seq_len(cmc_num_shards))

colnames(cmc_imh_post_burn_in_acceptance_rates_by_shard) <-
  paste0("Chain_", seq_len(num_chains))

cmc_imh_mean_post_burn_in_acceptance_rates <- rowMeans(
  cmc_imh_post_burn_in_acceptance_rates_by_shard
)

cmc_imh_gelman_diag_list <- lapply(
  cmc_imh_results,
  function(result) result$gelman_diag
)

cmc_imh_shard_ess_list <- lapply(
  cmc_imh_results,
  function(result) result$ess_by_parameter
)

cmc_imh_shard_ess_table <- do.call(
  rbind,
  lapply(
    seq_along(cmc_imh_shard_ess_list),
    function(k) {
      ess_k <- cmc_imh_shard_ess_list[[k]]

      data.frame(
        Method = "CMC-IMH",
        Shard = k,
        Parameter = names(ess_k),
        ESS = as.numeric(ess_k),
        row.names = NULL
      )
    }
  )
)

for (k in seq_len(cmc_num_shards)) {
  cat(
    "Shard", k,
    "CMC-IMH mean post-burn-in acceptance rate:",
    cmc_imh_mean_post_burn_in_acceptance_rates[k],
    "\n"
  )
}

# For CMC, chain identity is preserved through the consensus step.
# Final CMC chain c is obtained by combining shard chain c from all shards.
# The four final consensus chains are used as a coda::mcmc.list for
# Gelman-Rubin diagnostics and ESS calculation.
# The pooled final consensus sample matrix is used only for posterior
# summaries, RMSE, and density plots.
cmc_imh_consensus_samples_list <- lapply(
  seq_len(num_chains),
  function(chain_id) {
    shard_samples_for_chain <- lapply(
      cmc_imh_results,
      function(result) {
        result$post_samples_list[[chain_id]]
      }
    )

    combine_consensus_samples(
      subposterior_samples_list = shard_samples_for_chain,
      param_names = param_names
    )
  }
)

cmc_imh_consensus_mcmc_list <- coda::mcmc.list(
  lapply(cmc_imh_consensus_samples_list, coda::mcmc)
)

cmc_imh_consensus_gelman_diag <- coda::gelman.diag(
  cmc_imh_consensus_mcmc_list,
  autoburnin = FALSE
)

cmc_imh_ess_by_parameter <- coda::effectiveSize(
  cmc_imh_consensus_mcmc_list
)

cmc_imh_ess_table <- data.frame(
  Method = "CMC-IMH",
  Parameter = names(cmc_imh_ess_by_parameter),
  ESS = as.numeric(cmc_imh_ess_by_parameter),
  row.names = NULL
)

# The final CMC-IMH posterior sample matrix is pooled from four
# final consensus chains. It is used for posterior summaries only.
# ESS and Rhat are computed from cmc_imh_consensus_mcmc_list.
cmc_imh_samples <- do.call(
  rbind,
  cmc_imh_consensus_samples_list
)

# End runtime measurement
cmc_imh_end_time <- Sys.time()

cmc_imh_runtime <- as.numeric(
  difftime(cmc_imh_end_time, cmc_imh_start_time, units = "secs")
)

runtime_table <- rbind(
  runtime_table,
  data.frame(
    Method = "CMC-IMH",
    Runtime_Seconds = cmc_imh_runtime,
    Runtime_Minutes = cmc_imh_runtime / 60
  )
)

# Posterior summary for CMC-IMH
cmc_imh_post_stats_table <- posterior_statistics(
  post_samples = cmc_imh_samples,
  ess = cmc_imh_ess_by_parameter
)

cat("\nCMC-IMH final consensus Gelman-Rubin diagnostics:\n")
print(
  gelman_psrf_table(
    cmc_imh_consensus_gelman_diag,
    "CMC-IMH"
  ),
  digits = 8
)

cat("\nCMC-IMH final consensus ESS by parameter:\n")
print(cmc_imh_ess_table)

cat("\nCMC-IMH shard-level ESS by parameter:\n")
print(cmc_imh_shard_ess_table)

print(cmc_imh_post_stats_table)

# RMSE of posterior mean
# The Full-data IMH posterior mean is used as the reference.
full_data_imh_posterior_mean <- colMeans(full_data_imh_post_samples)
cmc_imh_posterior_mean <- colMeans(cmc_imh_samples)

cmc_imh_posterior_mean_error <-
  cmc_imh_posterior_mean - full_data_imh_posterior_mean

cmc_imh_posterior_mean_rmse <- sqrt(
  mean(cmc_imh_posterior_mean_error^2)
)

cmc_imh_rmse_table <- data.frame(
  Method = "CMC-IMH",
  Reference = "Full-data IMH",
  Posterior_Mean_RMSE = cmc_imh_posterior_mean_rmse
)

print(cmc_imh_rmse_table)

# Parameter-wise posterior mean error
cmc_imh_posterior_mean_error_table <- data.frame(
  Parameter = param_names,
  Full_Data_IMH_Mean = full_data_imh_posterior_mean,
  CMC_IMH_Mean = cmc_imh_posterior_mean,
  Error = cmc_imh_posterior_mean_error,
  Squared_Error = cmc_imh_posterior_mean_error^2,
  row.names = NULL
)

print(cmc_imh_posterior_mean_error_table)

# Density plots for consensus samples
# These samples are produced by combining shard subposterior samples,
# not by running one single Markov chain.
consensus_density_plot(cmc_imh_samples)

# Trace and ACF plots use one representative final consensus chain.
# This avoids plotting time-series diagnostics from pooled samples.
cmc_imh_plot_chain_id <- 1

hist_trace_plot(
  cmc_imh_consensus_samples_list[[cmc_imh_plot_chain_id]]
)

acf_mcmc_plot(
  cmc_imh_consensus_samples_list[[cmc_imh_plot_chain_id]]
)


# 12. Compare all methods

cat("\nFull-data RWMH chain post-burn-in acceptance rates:\n")
print(full_data_rwmh_post_burn_in_acceptance_rates)

cat("\nMean Full-data RWMH post-burn-in acceptance rate:\n")
print(mean(full_data_rwmh_post_burn_in_acceptance_rates))

cat("\nFull-data IMH chain post-burn-in acceptance rates:\n")
print(full_data_imh_post_burn_in_acceptance_rates)

cat("\nMean Full-data IMH post-burn-in acceptance rate:\n")
print(mean(full_data_imh_post_burn_in_acceptance_rates))

cat("\nCMC-RWMH shard-by-chain post-burn-in acceptance rates:\n")
print(cmc_rwmh_post_burn_in_acceptance_rates_by_shard)

cat("\nCMC-RWMH mean post-burn-in acceptance rate by shard:\n")
print(cmc_rwmh_mean_post_burn_in_acceptance_rates)

cat("\nCMC-IMH shard-by-chain post-burn-in acceptance rates:\n")
print(cmc_imh_post_burn_in_acceptance_rates_by_shard)

cat("\nCMC-IMH mean post-burn-in acceptance rate by shard:\n")
print(cmc_imh_mean_post_burn_in_acceptance_rates)

cat("\nFull-data RWMH Gelman-Rubin diagnostics:\n")
print(gelman_psrf_table(full_data_rwmh_gelman_diag, "Full-data RWMH"),
      digits = 8)

cat("\nFull-data IMH Gelman-Rubin diagnostics:\n")
print(gelman_psrf_table(full_data_imh_gelman_diag, "Full-data IMH"),
      digits = 8)

cat("\nCMC-RWMH shard Gelman-Rubin diagnostics:\n")
for (k in seq_along(cmc_rwmh_gelman_diag_list)) {
  cat("\nCMC-RWMH shard", k, "Gelman-Rubin diagnostics:\n")
  print(
    gelman_psrf_table(
      cmc_rwmh_gelman_diag_list[[k]],
      paste0("CMC-RWMH Shard ", k)
    ),
    digits = 8
  )
}

cat("\nCMC-RWMH final consensus Gelman-Rubin diagnostics:\n")
print(
  gelman_psrf_table(
    cmc_rwmh_consensus_gelman_diag,
    "CMC-RWMH"
  ),
  digits = 8
)

cat("\nCMC-IMH shard Gelman-Rubin diagnostics:\n")
for (k in seq_along(cmc_imh_gelman_diag_list)) {
  cat("\nCMC-IMH shard", k, "Gelman-Rubin diagnostics:\n")
  print(
    gelman_psrf_table(
      cmc_imh_gelman_diag_list[[k]],
      paste0("CMC-IMH Shard ", k)
    ),
    digits = 8
  )
}

cat("\nCMC-IMH final consensus Gelman-Rubin diagnostics:\n")
print(
  gelman_psrf_table(
    cmc_imh_consensus_gelman_diag,
    "CMC-IMH"
  ),
  digits = 8
)

cat("\nFull-data RWMH ESS and MCSE:\n")
print(
  full_data_rwmh_post_stats_table[, c("Parameter", "ESS", "MCSE")]
)

cat("\nFull-data IMH ESS and MCSE:\n")
print(
  full_data_imh_post_stats_table[, c("Parameter", "ESS", "MCSE")]
)

cat("\nCMC-RWMH ESS and MCSE:\n")
print(
  cmc_rwmh_post_stats_table[, c("Parameter", "ESS", "MCSE")]
)

cat("\nCMC-IMH ESS and MCSE:\n")
print(
  cmc_imh_post_stats_table[, c("Parameter", "ESS", "MCSE")]
)


cat("\nPosterior Summary: Full-data RWMH\n")
print(full_data_rwmh_post_stats_table)

cat("\nPosterior Summary: Full-data IMH\n")
print(full_data_imh_post_stats_table)

cat("\nPosterior Summary: CMC-RWMH\n")
print(cmc_rwmh_post_stats_table)

cat("\nPosterior Summary: CMC-IMH\n")
print(cmc_imh_post_stats_table)


cat("\nRuntime Comparison:\n")
print(runtime_table)
