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

report_artifact_manifest <- data.frame(
  Artifact = character(),
  File_Path = character(),
  Report_Section = character(),
  Description = character(),
  stringsAsFactors = FALSE
)

infer_report_section <- function(file_path) {
  file_name <- basename(file_path)

  if (grepl("^data_preview|response_distribution|road_type_distribution|speed_limit_distribution|reference_categories|parameter_names|design_matrix_info|cmc_shard_", file_name)) {
    return("Data and preprocessing")
  }

  if (grepl("^frequentist_|^pearson_residual|^deviance_residual", file_name)) {
    return("Frequentist logistic regression")
  }

  if (grepl("^model_settings|^proposal_cov_", file_name)) {
    return("Bayesian model settings")
  }

  if (grepl("posterior_summary|posterior_mean_comparison|ess_mcse_comparison|odds_ratio_comparison", file_name)) {
    return("Posterior summaries")
  }

  if (grepl("acceptance_rates", file_name)) {
    return("MCMC diagnostics")
  }

  if (grepl("^cmc_.*rmse|posterior_mean_error", file_name)) {
    return("CMC approximation accuracy")
  }

  if (grepl("runtime_comparison", file_name)) {
    return("Runtime comparison")
  }

  if (grepl("\\.pdf$", file_name)) {
    return("Figures")
  }

  return("CMC diagnostics")
}

infer_artifact_description <- function(file_path) {
  file_name <- basename(file_path)
  description <- tools::file_path_sans_ext(file_name)
  description <- gsub("_%02d$", "", description)
  description <- gsub("_", " ", description)
  description <- tools::toTitleCase(description)
  description
}

register_artifact <- function(file_path, report_section = NULL, description = NULL) {
  if (is.null(report_section)) {
    report_section <- infer_report_section(file_path)
  }

  if (is.null(description)) {
    description <- infer_artifact_description(file_path)
  }

  report_artifact_manifest <<- unique(
    rbind(
      report_artifact_manifest,
      data.frame(
        Artifact = tools::file_path_sans_ext(basename(file_path)),
        File_Path = file_path,
        Report_Section = report_section,
        Description = description,
        stringsAsFactors = FALSE
      )
    )
  )
}

safe_write_csv <- function(object, file_path, report_section = NULL, description = NULL) {
  if (!is.null(object)) {
    write.csv(object, file_path, row.names = FALSE)
    register_artifact(file_path, report_section, description)
  }
}

safe_write_matrix <- function(object, file_path, report_section = NULL, description = NULL) {
  if (!is.null(object)) {
    write.csv(as.data.frame(object), file_path, row.names = TRUE)
    register_artifact(file_path, report_section, description)
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

if (exists("full_data_rwmh_post_stats_table")) {
  safe_write_csv(
    full_data_rwmh_post_stats_table,
    "Results/full_data_rwmh_posterior_summary.csv"
  )
}

if (exists("full_data_imh_post_stats_table")) {
  safe_write_csv(
    full_data_imh_post_stats_table,
    "Results/full_data_imh_posterior_summary.csv"
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

if (exists("full_data_rwmh_post_stats_table")) {
  posterior_summary_objects[["Full_Data_RWMH"]] <- full_data_rwmh_post_stats_table
}

if (exists("full_data_imh_post_stats_table")) {
  posterior_summary_objects[["Full_Data_IMH"]] <- full_data_imh_post_stats_table
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

# Consensus Monte Carlo approximation accuracy exports
rmse_objects <- list()

if (exists("cmc_rwmh_rmse_table")) {
  safe_write_csv(
    cmc_rwmh_rmse_table,
    "Results/cmc_rwmh_rmse.csv"
  )

  rmse_objects[["CMC_RWMH"]] <- cmc_rwmh_rmse_table
}

if (exists("cmc_imh_rmse_table")) {
  safe_write_csv(
    cmc_imh_rmse_table,
    "Results/cmc_imh_rmse.csv"
  )

  rmse_objects[["CMC_IMH"]] <- cmc_imh_rmse_table
}

if (length(rmse_objects) > 0) {
  cmc_rmse_comparison <- do.call(rbind, rmse_objects)
  row.names(cmc_rmse_comparison) <- NULL

  safe_write_csv(
    cmc_rmse_comparison,
    "Results/cmc_rmse_comparison.csv"
  )
}

if (exists("cmc_rwmh_posterior_mean_error_table")) {
  safe_write_csv(
    cmc_rwmh_posterior_mean_error_table,
    "Results/cmc_rwmh_posterior_mean_error.csv"
  )
}

if (exists("cmc_imh_posterior_mean_error_table")) {
  safe_write_csv(
    cmc_imh_posterior_mean_error_table,
    "Results/cmc_imh_posterior_mean_error.csv"
  )
}

# -------------------------------------------------------------------
# 5. Acceptance rate exports
# -------------------------------------------------------------------

acceptance_list <- list()

if (exists("full_data_rwmh_result")) {
  acceptance_list[["Full-data RWMH"]] <-
    full_data_rwmh_result$post_burn_in_acceptance_rate
}

if (exists("full_data_imh_result")) {
  acceptance_list[["Full-data IMH"]] <-
    full_data_imh_result$post_burn_in_acceptance_rate
}

if (exists("cmc_rwmh_post_burn_in_acceptance_rates")) {
  for (i in seq_along(cmc_rwmh_post_burn_in_acceptance_rates)) {
    acceptance_list[[paste0("CMC-RWMH Shard ", i)]] <-
      cmc_rwmh_post_burn_in_acceptance_rates[i]
  }

  acceptance_list[["Mean CMC-RWMH"]] <-
    mean(cmc_rwmh_post_burn_in_acceptance_rates)
}

if (exists("cmc_imh_post_burn_in_acceptance_rates")) {
  for (i in seq_along(cmc_imh_post_burn_in_acceptance_rates)) {
    acceptance_list[[paste0("CMC-IMH Shard ", i)]] <-
      cmc_imh_post_burn_in_acceptance_rates[i]
  }

  acceptance_list[["Mean CMC-IMH"]] <-
    mean(cmc_imh_post_burn_in_acceptance_rates)
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
  runtime_table_export <- runtime_table

  safe_write_csv(
    runtime_table_export,
    "Results/runtime_comparison.csv"
  )
}

# -------------------------------------------------------------------
# 7. Consensus Monte Carlo shard diagnostics
# -------------------------------------------------------------------

if (exists("cmc_shard_id")) {
  shard_sizes <- as.data.frame(table(cmc_shard_id))
  names(shard_sizes) <- c("Shard", "Count")

  safe_write_csv(
    shard_sizes,
    "Results/cmc_shard_sizes.csv"
  )
}

if (exists("cmc_shard_id") && exists("y")) {
  shard_response_distribution <- as.data.frame(
    table(cmc_shard_id, y)
  )
  names(shard_response_distribution) <- c(
    "Shard",
    "Response",
    "Count"
  )

  safe_write_csv(
    shard_response_distribution,
    "Results/cmc_shard_response_distribution.csv"
  )
}

if (exists("cmc_shard_id") && exists("data_used") && "road_type" %in% names(data_used)) {
  shard_road_type_distribution <- as.data.frame(
    table(cmc_shard_id, data_used$road_type)
  )
  names(shard_road_type_distribution) <- c(
    "Shard",
    "Road_Type",
    "Count"
  )

  safe_write_csv(
    shard_road_type_distribution,
    "Results/cmc_shard_road_type_distribution.csv"
  )
}

if (exists("cmc_shard_id") && exists("data_used") && "speed_limit" %in% names(data_used)) {
  shard_speed_limit_distribution <- as.data.frame(
    table(cmc_shard_id, data_used$speed_limit)
  )
  names(shard_speed_limit_distribution) <- c(
    "Shard",
    "Speed_Limit",
    "Count"
  )

  safe_write_csv(
    shard_speed_limit_distribution,
    "Results/cmc_shard_speed_limit_distribution.csv"
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

if (exists("cmc_num_shards")) {
  settings <- rbind(
    settings,
    add_setting("Number of shards", cmc_num_shards)
  )
}

if (exists("cmc_iterations")) {
  settings <- rbind(
    settings,
    add_setting("CMC shard MCMC iterations", cmc_iterations)
  )
}

if (exists("cmc_burn_in")) {
  settings <- rbind(
    settings,
    add_setting("CMC shard burn-in iterations", cmc_burn_in)
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

graphics.off()

fig_dpi <- 100

fig_width_px <- 800
fig_height_px <- 600
fig_width_in <- fig_width_px / fig_dpi
fig_height_in <- fig_height_px / fig_dpi

fig_wide_width_px <- 800
fig_wide_height_px <- 300
fig_wide_width_in <- fig_wide_width_px / fig_dpi
fig_wide_height_in <- fig_wide_height_px / fig_dpi

# Full-data RWMH histogram and trace plots
if (exists("full_data_rwmh_post_samples") && exists("hist_trace_plot")) {
  pdf(
    "Figure/full_data_rwmh_hist_trace_%02d.pdf",
    width = fig_width_in,
    height = fig_height_in,
    onefile = FALSE
  )

  old_par <- par(no.readonly = TRUE)
  par(
    cex.main = 0.80,
    cex.lab = 0.85,
    cex.axis = 0.85
  )

  hist_trace_plot(full_data_rwmh_post_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/full_data_rwmh_hist_trace_%02d.pdf",
    "Figures",
    "Full-data RWMH posterior histograms and trace plots"
  )
}

# Full-data RWMH ACF plots
if (exists("full_data_rwmh_post_samples") && exists("acf_mcmc_plot")) {
  pdf(
    "Figure/full_data_rwmh_acf_%02d.pdf",
    width = fig_wide_width_in,
    height = fig_wide_height_in,
    onefile = FALSE
  )

  old_par <- par(no.readonly = TRUE)
  par(
    cex.main = 0.75,
    cex.lab = 0.80,
    cex.axis = 0.80
  )

  acf_mcmc_plot(full_data_rwmh_post_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/full_data_rwmh_acf_%02d.pdf",
    "Figures",
    "Full-data RWMH posterior autocorrelation plots"
  )
}

# Full-data IMH histogram and trace plots
if (exists("full_data_imh_post_samples") && exists("hist_trace_plot")) {
  pdf(
    "Figure/full_data_imh_hist_trace_%02d.pdf",
    width = fig_width_in,
    height = fig_height_in,
    onefile = FALSE
  )

  old_par <- par(no.readonly = TRUE)
  par(
    cex.main = 0.80,
    cex.lab = 0.85,
    cex.axis = 0.85
  )

  hist_trace_plot(full_data_imh_post_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/full_data_imh_hist_trace_%02d.pdf",
    "Figures",
    "Full-data IMH posterior histograms and trace plots"
  )
}

# Full-data IMH ACF plots
if (exists("full_data_imh_post_samples") && exists("acf_mcmc_plot")) {
  pdf(
    "Figure/full_data_imh_acf_%02d.pdf",
    width = fig_wide_width_in,
    height = fig_wide_height_in,
    onefile = FALSE
  )

  old_par <- par(no.readonly = TRUE)
  par(
    cex.main = 0.75,
    cex.lab = 0.80,
    cex.axis = 0.80
  )

  acf_mcmc_plot(full_data_imh_post_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/full_data_imh_acf_%02d.pdf",
    "Figures",
    "Full-data IMH posterior autocorrelation plots"
  )
}

# CMC-RWMH density plots
if (exists("cmc_rwmh_samples") && exists("consensus_density_plot")) {
  pdf(
    "Figure/cmc_rwmh_density_%02d.pdf",
    width = fig_wide_width_in,
    height = fig_wide_height_in,
    onefile = FALSE
  )

  old_par <- par(no.readonly = TRUE)
  par(
    cex.main = 0.65,
    cex.lab = 0.75,
    cex.axis = 0.75,
    mar = c(5, 4, 3, 1),
    oma = c(0, 0, 0, 0)
  )

  consensus_density_plot(cmc_rwmh_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/cmc_rwmh_density_%02d.pdf",
    "Figures",
    "CMC-RWMH consensus posterior density plots"
  )
}

# CMC-IMH density plots
if (exists("cmc_imh_samples") && exists("consensus_density_plot")) {
  pdf(
    "Figure/cmc_imh_density_%02d.pdf",
    width = fig_wide_width_in,
    height = fig_wide_height_in,
    onefile = FALSE
  )

  old_par <- par(no.readonly = TRUE)
  par(
    cex.main = 0.65,
    cex.lab = 0.75,
    cex.axis = 0.75,
    mar = c(5, 4, 3, 1),
    oma = c(0, 0, 0, 0)
  )

  consensus_density_plot(cmc_imh_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/cmc_imh_density_%02d.pdf",
    "Figures",
    "CMC-IMH consensus posterior density plots"
  )
}

if (exists("pearson_resid_std") && exists("deviance_resid_std")) {
  pdf(
    "Figure/residual_acf_correlograms.pdf",
    width = fig_wide_width_in,
    height = fig_wide_height_in
  )

  old_par <- par(no.readonly = TRUE)
  par(
    mfrow = c(1, 2),
    mar = c(4, 4, 3, 1),
    cex.main = 0.75,
    cex.lab = 0.80,
    cex.axis = 0.80
  )

  acf(
    pearson_resid_std,
    main = "Correlogram of Pearson Residuals"
  )

  acf(
    deviance_resid_std,
    main = "Correlogram of Deviance Residuals"
  )

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/residual_acf_correlograms.pdf",
    "Figures",
    "Residual autocorrelation correlograms"
  )
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
  "full_data_rwmh_result",
  "full_data_imh_result",
  "full_data_rwmh_chain",
  "full_data_imh_chain",
  "full_data_rwmh_post_samples",
  "full_data_imh_post_samples",
  "full_data_rwmh_post_stats_table",
  "full_data_imh_post_stats_table",
  "cmc_rwmh_results",
  "cmc_imh_results",
  "cmc_rwmh_subposterior_samples_list",
  "cmc_imh_subposterior_samples_list",
  "cmc_rwmh_post_burn_in_acceptance_rates",
  "cmc_imh_post_burn_in_acceptance_rates",
  "cmc_rwmh_samples",
  "cmc_imh_samples",
  "cmc_rwmh_post_stats_table",
  "cmc_imh_post_stats_table",
  "full_data_rwmh_posterior_mean",
  "full_data_imh_posterior_mean",
  "cmc_rwmh_posterior_mean",
  "cmc_imh_posterior_mean",
  "cmc_rwmh_rmse_table",
  "cmc_imh_rmse_table",
  "cmc_rwmh_posterior_mean_error_table",
  "cmc_imh_posterior_mean_error_table",
  "runtime_table",
  "proposal_cov_rwmh",
  "proposal_cov_imh",
  "cmc_rwmh_proposal_cov"
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

register_artifact(
  "Results/report_artifact_manifest.csv",
  "Data and preprocessing",
  "Manifest of report-ready tables and figures exported by the script"
)

write.csv(
  report_artifact_manifest,
  "Results/report_artifact_manifest.csv",
  row.names = FALSE
)

cat("Export complete. Results saved in Results/ and figures saved in Figure/.\n")
