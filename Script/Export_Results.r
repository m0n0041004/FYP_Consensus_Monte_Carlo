# Export results for thesis tables
# Run this after Consensus_Monte_Carlo.r has completed.

if (!dir.exists("Results")) {
  dir.create("Results")
}

# Full-data Random Walk MH
if (exists("rwmh_post_stats_table")) {
  write.csv(
    rwmh_post_stats_table,
    "Results/full_rwmh_posterior_summary.csv",
    row.names = FALSE
  )
}

# Full-data Independent MH
if (exists("imh_post_stats_table")) {
  write.csv(
    imh_post_stats_table,
    "Results/full_imh_posterior_summary.csv",
    row.names = FALSE
  )
}

# Consensus Monte Carlo using RWMH
if (exists("cmc_rwmh_post_stats_table")) {
  write.csv(
    cmc_rwmh_post_stats_table,
    "Results/cmc_rwmh_posterior_summary.csv",
    row.names = FALSE
  )
}

# Consensus Monte Carlo using IMH
if (exists("cmc_imh_post_stats_table")) {
  write.csv(
    cmc_imh_post_stats_table,
    "Results/cmc_imh_posterior_summary.csv",
    row.names = FALSE
  )
}

# Acceptance rates
acceptance_list <- list()

if (exists("rwmh_result")) {
  acceptance_list[["Full RWMH"]] <- rwmh_result$acceptance_rate
}

if (exists("imh_result")) {
  acceptance_list[["Full IMH"]] <- imh_result$acceptance_rate
}

if (exists("cmc_rwmh_acceptance_rates")) {
  for (i in seq_along(cmc_rwmh_acceptance_rates)) {
    acceptance_list[[paste0("CMC RWMH Subset ", i)]] <-
      cmc_rwmh_acceptance_rates[i]
  }
}

if (exists("cmc_imh_acceptance_rates")) {
  for (i in seq_along(cmc_imh_acceptance_rates)) {
    acceptance_list[[paste0("CMC IMH Subset ", i)]] <-
      cmc_imh_acceptance_rates[i]
  }
}

if (length(acceptance_list) > 0) {
  acceptance_rates <- data.frame(
    Method = names(acceptance_list),
    Acceptance_Rate = as.numeric(acceptance_list),
    row.names = NULL
  )

  write.csv(
    acceptance_rates,
    "Results/acceptance_rates.csv",
    row.names = FALSE
  )
}

# Model settings
settings <- data.frame(
  Setting = character(),
  Value = character()
)

add_setting <- function(name, value) {
  data.frame(
    Setting = name,
    Value = as.character(value)
  )
}

if (exists("data_used")) {
  settings <- rbind(settings, add_setting("Observations after preprocessing", nrow(data_used)))
}

if (exists("d")) {
  settings <- rbind(settings, add_setting("Number of coefficients", d))
}

if (exists("num_iterations")) {
  settings <- rbind(settings, add_setting("Full-data MCMC iterations", num_iterations))
}

if (exists("burn_in_fraction")) {
  settings <- rbind(settings, add_setting("Burn-in fraction", burn_in_fraction))
}

if (exists("cmc_num_subsets")) {
  settings <- rbind(settings, add_setting("Number of subsets", cmc_num_subsets))
}

if (exists("cmc_iterations")) {
  settings <- rbind(settings, add_setting("Subset MCMC iterations", cmc_iterations))
}

if (exists("settings") && nrow(settings) > 0) {
  write.csv(
    settings,
    "Results/model_settings.csv",
    row.names = FALSE
  )
}

cat("Export complete. Results saved in Results/.\n")