# Export results for thesis tables and figures
# Run this after Consensus_Monte_Carlo.r has completed or partially completed.

# -------------------------------------------------------------------
# 0. Create output folders
# -------------------------------------------------------------------

if (!dir.exists("Results")) {
  dir.create("Results")
}

if (!dir.exists("Figure")) {
  dir.create("Figure")
}

# -------------------------------------------------------------------
# Helper functions
# -------------------------------------------------------------------

safe_write_csv <- function(object, file_path) {
  if (!is.null(object)) {
    write.csv(object, file_path, row.names = FALSE)
  }
}

safe_write_matrix <- function(object, file_path) {
  if (!is.null(object)) {
    write.csv(as.data.frame(object), file_path, row.names = TRUE)
  }
}

add_setting <- function(name, value) {
  data.frame(
    Setting = name,
    Value = as.character(value),
    stringsAsFactors = FALSE
  )
}

residual_summary_table <- function(residuals) {
  residuals <- as.numeric(residuals)
  residuals <- residuals[is.finite(residuals)]

  data.frame(
    Minimum = min(residuals),
    First_Quartile = as.numeric(quantile(residuals, probs = 0.25)),
    Median = median(residuals),
    Mean = mean(residuals),
    Third_Quartile = as.numeric(quantile(residuals, probs = 0.75)),
    Maximum = max(residuals),
    Std_Dev = sd(residuals),
    row.names = NULL
  )
}

# -------------------------------------------------------------------
# 1. Data summary exports
# -------------------------------------------------------------------

# Final dataset preview
if (exists("data_used")) {
  safe_write_csv(
    head(data_used, 20),
    "Results/data_preview.csv"
  )
}

# Response distribution after preprocessing
if (exists("data_used") && "accident_severity_binary" %in% names(data_used)) {
  response_count <- as.data.frame(
    table(data_used$accident_severity_binary)
  )
  names(response_count) <- c("Accident_Severity_Binary", "Count")

  response_prop <- as.data.frame(
    prop.table(table(data_used$accident_severity_binary))
  )
  names(response_prop) <- c("Accident_Severity_Binary", "Proportion")

  response_distribution <- merge(
    response_count,
    response_prop,
    by = "Accident_Severity_Binary"
  )

  safe_write_csv(
    response_distribution,
    "Results/response_distribution.csv"
  )
}

# Road type distribution after preprocessing
if (exists("data_used") && "road_type" %in% names(data_used)) {
  road_type_count <- as.data.frame(table(data_used$road_type))
  names(road_type_count) <- c("Road_Type", "Count")

  road_type_prop <- as.data.frame(prop.table(table(data_used$road_type)))
  names(road_type_prop) <- c("Road_Type", "Proportion")

  road_type_distribution <- merge(
    road_type_count,
    road_type_prop,
    by = "Road_Type"
  )

  safe_write_csv(
    road_type_distribution,
    "Results/road_type_distribution.csv"
  )
}

# Speed limit distribution after preprocessing
if (exists("data_used") && "speed_limit" %in% names(data_used)) {
  speed_limit_count <- as.data.frame(table(data_used$speed_limit))
  names(speed_limit_count) <- c("Speed_Limit", "Count")

  speed_limit_prop <- as.data.frame(prop.table(table(data_used$speed_limit)))
  names(speed_limit_prop) <- c("Speed_Limit", "Proportion")

  speed_limit_distribution <- merge(
    speed_limit_count,
    speed_limit_prop,
    by = "Speed_Limit"
  )

  safe_write_csv(
    speed_limit_distribution,
    "Results/speed_limit_distribution.csv"
  )
}

# Reference categories
reference_categories <- data.frame(
  Variable = c("accident_severity_binary", "road_type", "speed_limit"),
  Reference_Category = c(
    if (exists("data_used") && "accident_severity_binary" %in% names(data_used)) {
      levels(data_used$accident_severity_binary)[1]
    } else {
      NA
    },
    if (exists("data_used") && "road_type" %in% names(data_used)) {
      levels(data_used$road_type)[1]
    } else {
      NA
    },
    if (exists("data_used") && "speed_limit" %in% names(data_used)) {
      levels(data_used$speed_limit)[1]
    } else {
      NA
    }
  ),
  stringsAsFactors = FALSE
)

safe_write_csv(
  reference_categories,
  "Results/reference_categories.csv"
)

# -------------------------------------------------------------------
# 2. Frequentist logistic regression exports
# -------------------------------------------------------------------

if (exists("logistic_model")) {
  glm_summary <- summary(logistic_model)$coefficients

  frequentist_logistic_summary <- data.frame(
    Parameter = rownames(glm_summary),
    Estimate = glm_summary[, "Estimate"],
    Std_Error = glm_summary[, "Std. Error"],
    Z_Value = glm_summary[, "z value"],
    P_Value = glm_summary[, "Pr(>|z|)"],
    row.names = NULL
  )

  safe_write_csv(
    frequentist_logistic_summary,
    "Results/frequentist_logistic_summary.csv"
  )
}

if (exists("g_square") && exists("g_square_df") && exists("p_value")) {
  likelihood_ratio_test <- data.frame(
    G_Square = g_square,
    DF = g_square_df,
    P_Value = p_value,
    row.names = NULL
  )

  safe_write_csv(
    likelihood_ratio_test,
    "Results/frequentist_likelihood_ratio_test.csv"
  )
}

if (exists("gvif_values")) {
  frequentist_gvif <- as.data.frame(gvif_values)
  frequentist_gvif <- data.frame(
    Variable = rownames(frequentist_gvif),
    frequentist_gvif,
    row.names = NULL,
    check.names = FALSE
  )

  if (all(c("GVIF", "Df") %in% names(frequentist_gvif))) {
    frequentist_gvif$GVIF_Adjusted <-
      frequentist_gvif$GVIF^(1 / (2 * frequentist_gvif$Df))
  }

  safe_write_csv(
    frequentist_gvif,
    "Results/frequentist_gvif.csv"
  )
}

if (exists("pearson_resid_std")) {
  pearson_residual_summary <- residual_summary_table(pearson_resid_std)

  safe_write_csv(
    pearson_residual_summary,
    "Results/pearson_residual_summary.csv"
  )
}

if (exists("deviance_resid_std")) {
  deviance_residual_summary <- residual_summary_table(deviance_resid_std)

  safe_write_csv(
    deviance_residual_summary,
    "Results/deviance_residual_summary.csv"
  )
}

if (exists("cov_beta")) {
  safe_write_matrix(
    cov_beta,
    "Results/frequentist_coefficient_covariance.csv"
  )
}

if (exists("cor_beta")) {
  safe_write_matrix(
    cor_beta,
    "Results/frequentist_coefficient_correlation.csv"
  )
}

# -------------------------------------------------------------------
# 3. Model matrix and parameter information
# -------------------------------------------------------------------

if (exists("param_names")) {
  parameter_table <- data.frame(
    Parameter_Index = seq_along(param_names),
    Parameter = param_names,
    stringsAsFactors = FALSE
  )

  safe_write_csv(
    parameter_table,
    "Results/parameter_names.csv"
  )
}

if (exists("X")) {
  design_matrix_info <- data.frame(
    Quantity = c("Number of observations", "Number of coefficients"),
    Value = c(nrow(X), ncol(X)),
    stringsAsFactors = FALSE
  )

  safe_write_csv(
    design_matrix_info,
    "Results/design_matrix_info.csv"
  )
}

# -------------------------------------------------------------------
# 4. Posterior summary exports
# -------------------------------------------------------------------

if (exists("rwmh_post_stats_table")) {
  safe_write_csv(
    rwmh_post_stats_table,
    "Results/full_rwmh_posterior_summary.csv"
  )
}

if (exists("imh_post_stats_table")) {
  safe_write_csv(
    imh_post_stats_table,
    "Results/full_imh_posterior_summary.csv"
  )
}

if (exists("cmc_rwmh_post_stats_table")) {
  safe_write_csv(
    cmc_rwmh_post_stats_table,
    "Results/cmc_rwmh_posterior_summary.csv"
  )
}

if (exists("cmc_imh_post_stats_table")) {
  safe_write_csv(
    cmc_imh_post_stats_table,
    "Results/cmc_imh_posterior_summary.csv"
  )
}

# Posterior mean comparison table
posterior_summary_objects <- list()

if (exists("rwmh_post_stats_table")) {
  posterior_summary_objects[["Full_RWMH"]] <- rwmh_post_stats_table
}

if (exists("imh_post_stats_table")) {
  posterior_summary_objects[["Full_IMH"]] <- imh_post_stats_table
}

if (exists("cmc_rwmh_post_stats_table")) {
  posterior_summary_objects[["CMC_RWMH"]] <- cmc_rwmh_post_stats_table
}

if (exists("cmc_imh_post_stats_table")) {
  posterior_summary_objects[["CMC_IMH"]] <- cmc_imh_post_stats_table
}

if (length(posterior_summary_objects) > 0) {
  posterior_mean_comparison <- NULL

  for (method_name in names(posterior_summary_objects)) {
    temp <- posterior_summary_objects[[method_name]][, c("Parameter", "Mean")]
    names(temp)[2] <- method_name

    if (is.null(posterior_mean_comparison)) {
      posterior_mean_comparison <- temp
    } else {
      posterior_mean_comparison <- merge(
        posterior_mean_comparison,
        temp,
        by = "Parameter",
        all = TRUE
      )
    }
  }

  safe_write_csv(
    posterior_mean_comparison,
    "Results/posterior_mean_comparison.csv"
  )
}

# ESS and MCSE comparison table
if (length(posterior_summary_objects) > 0) {
  ess_mcse_comparison <- NULL

  for (method_name in names(posterior_summary_objects)) {
    temp <- posterior_summary_objects[[method_name]][, c("Parameter", "ESS", "MCSE")]
    names(temp)[2:3] <- paste(method_name, c("ESS", "MCSE"), sep = "_")

    if (is.null(ess_mcse_comparison)) {
      ess_mcse_comparison <- temp
    } else {
      ess_mcse_comparison <- merge(
        ess_mcse_comparison,
        temp,
        by = "Parameter",
        all = TRUE
      )
    }
  }

  safe_write_csv(
    ess_mcse_comparison,
    "Results/ess_mcse_comparison.csv"
  )
}

# -------------------------------------------------------------------
# 5. Acceptance rate exports
# -------------------------------------------------------------------

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

  acceptance_list[["Mean CMC RWMH"]] <- mean(cmc_rwmh_acceptance_rates)
}

if (exists("cmc_imh_acceptance_rates")) {
  for (i in seq_along(cmc_imh_acceptance_rates)) {
    acceptance_list[[paste0("CMC IMH Subset ", i)]] <-
      cmc_imh_acceptance_rates[i]
  }

  acceptance_list[["Mean CMC IMH"]] <- mean(cmc_imh_acceptance_rates)
}

if (length(acceptance_list) > 0) {
  acceptance_rates <- data.frame(
    Method = names(acceptance_list),
    Acceptance_Rate = as.numeric(acceptance_list),
    row.names = NULL
  )

  safe_write_csv(
    acceptance_rates,
    "Results/acceptance_rates.csv"
  )
}

# -------------------------------------------------------------------
# 6. Runtime exports
# -------------------------------------------------------------------

if (exists("runtime_table")) {
  safe_write_csv(
    runtime_table,
    "Results/runtime_comparison.csv"
  )
}

# -------------------------------------------------------------------
# 7. Consensus Monte Carlo subset diagnostics
# -------------------------------------------------------------------

if (exists("cmc_subset_id")) {
  subset_sizes <- as.data.frame(table(cmc_subset_id))
  names(subset_sizes) <- c("Subset", "Count")

  safe_write_csv(
    subset_sizes,
    "Results/cmc_subset_sizes.csv"
  )
}

if (exists("cmc_subset_id") && exists("y")) {
  subset_response_distribution <- as.data.frame(
    table(cmc_subset_id, y)
  )
  names(subset_response_distribution) <- c(
    "Subset",
    "Response",
    "Count"
  )

  safe_write_csv(
    subset_response_distribution,
    "Results/cmc_subset_response_distribution.csv"
  )
}

if (exists("cmc_subset_id") && exists("data_used") && "road_type" %in% names(data_used)) {
  subset_road_type_distribution <- as.data.frame(
    table(cmc_subset_id, data_used$road_type)
  )
  names(subset_road_type_distribution) <- c(
    "Subset",
    "Road_Type",
    "Count"
  )

  safe_write_csv(
    subset_road_type_distribution,
    "Results/cmc_subset_road_type_distribution.csv"
  )
}

if (exists("cmc_subset_id") && exists("data_used") && "speed_limit" %in% names(data_used)) {
  subset_speed_limit_distribution <- as.data.frame(
    table(cmc_subset_id, data_used$speed_limit)
  )
  names(subset_speed_limit_distribution) <- c(
    "Subset",
    "Speed_Limit",
    "Count"
  )

  safe_write_csv(
    subset_speed_limit_distribution,
    "Results/cmc_subset_speed_limit_distribution.csv"
  )
}

# -------------------------------------------------------------------
# 8. MCMC and proposal settings
# -------------------------------------------------------------------

settings <- data.frame(
  Setting = character(),
  Value = character(),
  stringsAsFactors = FALSE
)

if (exists("data_used")) {
  settings <- rbind(
    settings,
    add_setting("Observations after preprocessing", nrow(data_used))
  )
}

if (exists("accident_data")) {
  settings <- rbind(
    settings,
    add_setting("Original observations", nrow(accident_data))
  )
}

if (exists("d")) {
  settings <- rbind(
    settings,
    add_setting("Number of coefficients", d)
  )
}

if (exists("prior_scale_value")) {
  settings <- rbind(
    settings,
    add_setting("Cauchy prior scale", prior_scale_value)
  )
}

if (exists("num_iterations")) {
  settings <- rbind(
    settings,
    add_setting("Full-data MCMC iterations", num_iterations)
  )
}

if (exists("burn_in_fraction")) {
  settings <- rbind(
    settings,
    add_setting("Burn-in fraction", burn_in_fraction)
  )
}

if (exists("burn_in")) {
  settings <- rbind(
    settings,
    add_setting("Full-data burn-in iterations", burn_in)
  )
}

if (exists("cmc_num_subsets")) {
  settings <- rbind(
    settings,
    add_setting("Number of subsets", cmc_num_subsets)
  )
}

if (exists("cmc_iterations")) {
  settings <- rbind(
    settings,
    add_setting("Subset MCMC iterations", cmc_iterations)
  )
}

if (exists("cmc_burn_in")) {
  settings <- rbind(
    settings,
    add_setting("Subset burn-in iterations", cmc_burn_in)
  )
}

if (exists("rwmh_tuning_factor")) {
  settings <- rbind(
    settings,
    add_setting("Full-data RWMH tuning factor", rwmh_tuning_factor)
  )
}

if (exists("imh_tuning_factor")) {
  settings <- rbind(
    settings,
    add_setting("Full-data IMH tuning factor", imh_tuning_factor)
  )
}

if (exists("cmc_rwmh_tuning_factor")) {
  settings <- rbind(
    settings,
    add_setting("CMC-RWMH tuning factor", cmc_rwmh_tuning_factor)
  )
}

if (exists("cmc_imh_tuning_factor")) {
  settings <- rbind(
    settings,
    add_setting("CMC-IMH tuning factor", cmc_imh_tuning_factor)
  )
}

settings <- rbind(
  settings,
  add_setting("Response event coded as 1", "serious_fatal"),
  add_setting("Response reference class coded as 0", "slight"),
  add_setting("Road type reference category", reference_categories$Reference_Category[reference_categories$Variable == "road_type"]),
  add_setting("Speed limit reference category", reference_categories$Reference_Category[reference_categories$Variable == "speed_limit"])
)

if (nrow(settings) > 0) {
  safe_write_csv(
    settings,
    "Results/model_settings.csv"
  )
}

# -------------------------------------------------------------------
# 9. Optional matrix exports for proposal covariance matrices
# -------------------------------------------------------------------

if (exists("proposal_cov_rwmh")) {
  safe_write_matrix(
    proposal_cov_rwmh,
    "Results/proposal_cov_full_rwmh.csv"
  )
}

if (exists("proposal_cov_imh")) {
  safe_write_matrix(
    proposal_cov_imh,
    "Results/proposal_cov_full_imh.csv"
  )
}

if (exists("cmc_rwmh_proposal_cov")) {
  safe_write_matrix(
    cmc_rwmh_proposal_cov,
    "Results/proposal_cov_cmc_rwmh.csv"
  )
}

# -------------------------------------------------------------------
# 10. Figure exports
# -------------------------------------------------------------------

# Full-data RWMH histogram and trace plots
if (exists("rwmh_post_samples") && exists("hist_trace_plot")) {
  png("Figure/full_rwmh_hist_trace_%02d.png", width = 800, height = 600)
  hist_trace_plot(rwmh_post_samples, params_per_page = 2)
  dev.off()
}

# Full-data IMH histogram and trace plots
if (exists("imh_post_samples") && exists("hist_trace_plot")) {
  png("Figure/full_imh_hist_trace_%02d.png", width = 800, height = 600)
  hist_trace_plot(imh_post_samples, params_per_page = 2)
  dev.off()
}

# CMC-RWMH density plots
if (exists("cmc_rwmh_samples") && exists("consensus_density_plot")) {
  png("Figure/cmc_rwmh_density_%02d.png", width = 800, height = 600)
  consensus_density_plot(cmc_rwmh_samples, params_per_page = 2)
  dev.off()
}

# CMC-IMH density plots
if (exists("cmc_imh_samples") && exists("consensus_density_plot")) {
  png("Figure/cmc_imh_density_%02d.png", width = 800, height = 600)
  consensus_density_plot(cmc_imh_samples, params_per_page = 2)
  dev.off()
}

if (exists("pearson_resid_std")) {
  png("Figure/pearson_residual_acf.png", width = 800, height = 600)
  acf(
    pearson_resid_std,
    main = "Correlogram of Pearson Residuals"
  )
  dev.off()
}

if (exists("deviance_resid_std")) {
  png("Figure/deviance_residual_acf.png", width = 800, height = 600)
  acf(
    deviance_resid_std,
    main = "Correlogram of Deviance Residuals"
  )
  dev.off()
}

# -------------------------------------------------------------------
# 11. Export object availability checklist
# -------------------------------------------------------------------

object_names <- c(
  "accident_data",
  "data_used",
  "logistic_model",
  "g_square",
  "g_square_df",
  "p_value",
  "gvif_values",
  "pearson_resid_std",
  "deviance_resid_std",
  "cov_beta",
  "cor_beta",
  "y",
  "X",
  "param_names",
  "prior_scale_value",
  "rwmh_result",
  "imh_result",
  "cmc_rwmh_acceptance_rates",
  "cmc_imh_acceptance_rates",
  "runtime_table",
  "rwmh_post_stats_table",
  "imh_post_stats_table",
  "cmc_rwmh_post_stats_table",
  "cmc_imh_post_stats_table",
  "rwmh_post_samples",
  "imh_post_samples",
  "cmc_rwmh_samples",
  "cmc_imh_samples"
)

object_availability <- data.frame(
  Object = object_names,
  Exists = sapply(object_names, exists),
  stringsAsFactors = FALSE
)

safe_write_csv(
  object_availability,
  "Results/export_object_availability.csv"
)

cat("Export complete. Results saved in Results/ and figures saved in Figure/.\n")
