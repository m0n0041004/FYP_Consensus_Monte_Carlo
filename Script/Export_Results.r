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

  if (grepl("acceptance_rates|gelman|rhat|ess|chain_dimension|chain_posterior_summary|diagnostic", file_name)) {
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

safe_save_rds <- function(object, file_path, report_section = NULL, description = NULL) {
  if (!is.null(object)) {
    saveRDS(object, file_path)
    register_artifact(file_path, report_section, description)
  }
}

object_exists <- function(object_name) {
  exists(object_name, inherits = TRUE)
}

object_value <- function(object_name) {
  if (object_exists(object_name)) {
    get(object_name, inherits = TRUE)
  } else {
    NULL
  }
}

valid_sample_matrix <- function(samples) {
  is.matrix(samples) &&
    nrow(samples) > 0 &&
    ncol(samples) > 0 &&
    all(is.finite(samples))
}

valid_sample_list <- function(samples_list) {
  if (!is.list(samples_list) || length(samples_list) == 0) {
    return(FALSE)
  }

  matrix_check <- vapply(samples_list, valid_sample_matrix, logical(1))

  if (!all(matrix_check)) {
    return(FALSE)
  }

  ncols <- vapply(samples_list, ncol, integer(1))
  identical_dimensions <- length(unique(ncols)) == 1

  colname_check <- vapply(
    samples_list,
    function(samples) {
      identical(colnames(samples), colnames(samples_list[[1]]))
    },
    logical(1)
  )

  identical_dimensions && all(colname_check)
}

ensure_valid_sample_list <- function(samples_list, object_name) {
  if (!valid_sample_list(samples_list)) {
    warning(
      object_name,
      " is missing, empty, non-finite, or has inconsistent chain dimensions.",
      call. = FALSE
    )
    return(FALSE)
  }

  TRUE
}

chain_dimension_table <- function(samples_list, method_name) {
  if (!valid_sample_list(samples_list)) {
    return(NULL)
  }

  data.frame(
    Method = method_name,
    Chain = seq_along(samples_list),
    Iterations = vapply(samples_list, nrow, integer(1)),
    Parameters = vapply(samples_list, ncol, integer(1)),
    stringsAsFactors = FALSE
  )
}

chain_posterior_summary_table <- function(samples_list, method_name) {
  if (!valid_sample_list(samples_list)) {
    return(NULL)
  }

  summaries <- lapply(
    seq_along(samples_list),
    function(chain_id) {
      samples <- samples_list[[chain_id]]

      data.frame(
        Method = method_name,
        Chain = chain_id,
        Parameter = colnames(samples),
        Mean = colMeans(samples),
        Median = apply(samples, 2, median),
        Std_Dev = apply(samples, 2, sd),
        CrI_95_Lower = apply(samples, 2, quantile, probs = 0.025),
        CrI_95_Upper = apply(samples, 2, quantile, probs = 0.975),
        Odds_Ratio = exp(colMeans(samples)),
        row.names = NULL
      )
    }
  )

  do.call(rbind, summaries)
}

gelman_table_from_object <- function(gelman_object, method_name) {
  if (is.null(gelman_object)) {
    return(NULL)
  }

  if (exists("gelman_psrf_table")) {
    return(gelman_psrf_table(gelman_object, method_name))
  }

  if (is.null(gelman_object$psrf)) {
    warning("Gelman-Rubin object for ", method_name, " does not contain psrf.", call. = FALSE)
    return(NULL)
  }

  psrf <- as.data.frame(gelman_object$psrf)
  psrf$Parameter <- rownames(psrf)
  rownames(psrf) <- NULL

  point_col <- grep("^Point", names(psrf), value = TRUE)
  upper_col <- grep("^Upper", names(psrf), value = TRUE)

  if (length(point_col) != 1 || length(upper_col) != 1) {
    warning("Gelman-Rubin object for ", method_name, " has an unexpected column structure.", call. = FALSE)
    return(NULL)
  }

  data.frame(
    Method = method_name,
    Parameter = psrf$Parameter,
    Rhat = psrf[[point_col]],
    Rhat_Upper_CI = psrf[[upper_col]],
    row.names = NULL
  )
}

gelman_table_from_list <- function(gelman_list, method_name) {
  if (!is.list(gelman_list) || length(gelman_list) == 0) {
    return(NULL)
  }

  tables <- lapply(
    seq_along(gelman_list),
    function(index) {
      gelman_table_from_object(
        gelman_list[[index]],
        paste0(method_name, " Shard ", index)
      )
    }
  )

  tables <- Filter(Negate(is.null), tables)

  if (length(tables) == 0) {
    return(NULL)
  }

  do.call(rbind, tables)
}

ess_table_from_vector <- function(ess_vector, method_name) {
  if (is.null(ess_vector)) {
    return(NULL)
  }

  data.frame(
    Method = method_name,
    Parameter = names(ess_vector),
    ESS = as.numeric(ess_vector),
    row.names = NULL
  )
}

acceptance_vector_table <- function(values, method_name, level = "Chain") {
  if (is.null(values)) {
    return(NULL)
  }

  values <- as.numeric(values)

  data.frame(
    Method = method_name,
    Level = level,
    Shard = NA_integer_,
    Chain = seq_along(values),
    Acceptance_Rate = values,
    row.names = NULL
  )
}

acceptance_matrix_table <- function(values, method_name) {
  if (is.null(values)) {
    return(NULL)
  }

  values <- as.matrix(values)

  output <- expand.grid(
    Shard = seq_len(nrow(values)),
    Chain = seq_len(ncol(values))
  )

  output$Method <- method_name
  output$Level <- "Shard-chain"
  output$Acceptance_Rate <- as.numeric(values[cbind(output$Shard, output$Chain)])

  output[, c("Method", "Level", "Shard", "Chain", "Acceptance_Rate")]
}

representative_samples <- function(samples_list_name,
                                   pooled_samples_name,
                                   chain_id_name = NULL) {
  samples_list <- object_value(samples_list_name)

  if (valid_sample_list(samples_list)) {
    chain_id <- 1

    if (!is.null(chain_id_name) && object_exists(chain_id_name)) {
      candidate_id <- as.integer(object_value(chain_id_name)[1])

      if (!is.na(candidate_id) &&
          candidate_id >= 1 &&
          candidate_id <= length(samples_list)) {
        chain_id <- candidate_id
      }
    }

    return(samples_list[[chain_id]])
  }

  pooled_samples <- object_value(pooled_samples_name)

  if (valid_sample_matrix(pooled_samples)) {
    return(pooled_samples)
  }

  NULL
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

# Multi-chain posterior summaries. Pooled sample matrices are exported in
# compact RDS format, while CSV files contain report-ready chain summaries.
multi_chain_sample_objects <- list(
  Full_Data_RWMH = list(
    samples_list = "full_data_rwmh_post_samples_list",
    pooled_samples = "full_data_rwmh_post_samples",
    description = "Full-data RWMH"
  ),
  Full_Data_IMH = list(
    samples_list = "full_data_imh_post_samples_list",
    pooled_samples = "full_data_imh_post_samples",
    description = "Full-data IMH"
  ),
  CMC_RWMH = list(
    samples_list = "cmc_rwmh_consensus_samples_list",
    pooled_samples = "cmc_rwmh_samples",
    description = "CMC-RWMH final consensus"
  ),
  CMC_IMH = list(
    samples_list = "cmc_imh_consensus_samples_list",
    pooled_samples = "cmc_imh_samples",
    description = "CMC-IMH final consensus"
  )
)

for (method_key in names(multi_chain_sample_objects)) {
  sample_info <- multi_chain_sample_objects[[method_key]]
  samples_list <- object_value(sample_info$samples_list)
  file_prefix <- tolower(method_key)

  if (!is.null(samples_list)) {
    if (ensure_valid_sample_list(samples_list, sample_info$samples_list)) {
      safe_write_csv(
        chain_dimension_table(samples_list, sample_info$description),
        file.path("Results", paste0(file_prefix, "_chain_dimensions.csv")),
        "MCMC diagnostics",
        paste(sample_info$description, "chain dimensions")
      )

      safe_write_csv(
        chain_posterior_summary_table(samples_list, sample_info$description),
        file.path("Results", paste0(file_prefix, "_chain_posterior_summary.csv")),
        "Posterior summaries",
        paste(sample_info$description, "chain-specific posterior summaries")
      )

      safe_save_rds(
        samples_list,
        file.path("Results", paste0(file_prefix, "_post_samples_list.rds")),
        "MCMC diagnostics",
        paste(sample_info$description, "post-burn-in samples by chain")
      )
    }
  }

  pooled_samples <- object_value(sample_info$pooled_samples)

  if (valid_sample_matrix(pooled_samples)) {
    safe_save_rds(
      pooled_samples,
      file.path("Results", paste0(file_prefix, "_pooled_post_samples.rds")),
      "Posterior summaries",
      paste(sample_info$description, "pooled posterior samples")
    )
  }
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

# Gelman-Rubin and ESS diagnostics for multi-chain workflows
gelman_tables <- list(
  gelman_table_from_object(
    object_value("full_data_rwmh_gelman_diag"),
    "Full-data RWMH"
  ),
  gelman_table_from_object(
    object_value("full_data_imh_gelman_diag"),
    "Full-data IMH"
  ),
  gelman_table_from_object(
    object_value("cmc_rwmh_consensus_gelman_diag"),
    "CMC-RWMH"
  ),
  gelman_table_from_object(
    object_value("cmc_imh_consensus_gelman_diag"),
    "CMC-IMH"
  )
)

gelman_tables <- Filter(Negate(is.null), gelman_tables)

if (length(gelman_tables) > 0) {
  safe_write_csv(
    do.call(rbind, gelman_tables),
    "Results/gelman_rubin_diagnostics.csv",
    "MCMC diagnostics",
    "Gelman-Rubin Rhat diagnostics for final multi-chain posterior samples"
  )
}

shard_gelman_tables <- list(
  gelman_table_from_list(
    object_value("cmc_rwmh_gelman_diag_list"),
    "CMC-RWMH"
  ),
  gelman_table_from_list(
    object_value("cmc_imh_gelman_diag_list"),
    "CMC-IMH"
  )
)

shard_gelman_tables <- Filter(Negate(is.null), shard_gelman_tables)

if (length(shard_gelman_tables) > 0) {
  safe_write_csv(
    do.call(rbind, shard_gelman_tables),
    "Results/cmc_shard_gelman_rubin_diagnostics.csv",
    "MCMC diagnostics",
    "CMC shard-level Gelman-Rubin Rhat diagnostics"
  )
}

ess_tables <- list(
  object_value("full_data_rwmh_ess_table"),
  object_value("full_data_imh_ess_table"),
  object_value("cmc_rwmh_ess_table"),
  object_value("cmc_imh_ess_table")
)

if (is.null(ess_tables[[1]])) {
  ess_tables[[1]] <- ess_table_from_vector(
    object_value("full_data_rwmh_ess_by_parameter"),
    "Full-data RWMH"
  )
}

if (is.null(ess_tables[[2]])) {
  ess_tables[[2]] <- ess_table_from_vector(
    object_value("full_data_imh_ess_by_parameter"),
    "Full-data IMH"
  )
}

if (is.null(ess_tables[[3]])) {
  ess_tables[[3]] <- ess_table_from_vector(
    object_value("cmc_rwmh_ess_by_parameter"),
    "CMC-RWMH"
  )
}

if (is.null(ess_tables[[4]])) {
  ess_tables[[4]] <- ess_table_from_vector(
    object_value("cmc_imh_ess_by_parameter"),
    "CMC-IMH"
  )
}

ess_tables <- Filter(Negate(is.null), ess_tables)

if (length(ess_tables) > 0) {
  safe_write_csv(
    do.call(rbind, ess_tables),
    "Results/effective_sample_size.csv",
    "MCMC diagnostics",
    "Effective sample size by method and parameter"
  )
}

shard_ess_tables <- Filter(
  Negate(is.null),
  list(
    object_value("cmc_rwmh_shard_ess_table"),
    object_value("cmc_imh_shard_ess_table")
  )
)

if (length(shard_ess_tables) > 0) {
  safe_write_csv(
    do.call(rbind, shard_ess_tables),
    "Results/cmc_shard_effective_sample_size.csv",
    "MCMC diagnostics",
    "CMC shard-level effective sample size by parameter"
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

acceptance_tables <- list(
  acceptance_vector_table(
    object_value("full_data_rwmh_post_burn_in_acceptance_rates"),
    "Full-data RWMH"
  ),
  acceptance_vector_table(
    object_value("full_data_imh_post_burn_in_acceptance_rates"),
    "Full-data IMH"
  ),
  acceptance_matrix_table(
    object_value("cmc_rwmh_post_burn_in_acceptance_rates_by_shard"),
    "CMC-RWMH"
  ),
  acceptance_matrix_table(
    object_value("cmc_imh_post_burn_in_acceptance_rates_by_shard"),
    "CMC-IMH"
  )
)

acceptance_tables <- Filter(Negate(is.null), acceptance_tables)

# Backward-compatible exports for the previous single-chain object structure.
if (length(acceptance_tables) == 0 && exists("full_data_rwmh_result")) {
  acceptance_list[["Full-data RWMH"]] <-
    full_data_rwmh_result$post_burn_in_acceptance_rate
}

if (length(acceptance_tables) == 0 && exists("full_data_imh_result")) {
  acceptance_list[["Full-data IMH"]] <-
    full_data_imh_result$post_burn_in_acceptance_rate
}

if (length(acceptance_tables) == 0 && exists("cmc_rwmh_post_burn_in_acceptance_rates")) {
  for (i in seq_along(cmc_rwmh_post_burn_in_acceptance_rates)) {
    acceptance_list[[paste0("CMC-RWMH Shard ", i)]] <-
      cmc_rwmh_post_burn_in_acceptance_rates[i]
  }

  acceptance_list[["Mean CMC-RWMH"]] <-
    mean(cmc_rwmh_post_burn_in_acceptance_rates)
}

if (length(acceptance_tables) == 0 && exists("cmc_imh_post_burn_in_acceptance_rates")) {
  for (i in seq_along(cmc_imh_post_burn_in_acceptance_rates)) {
    acceptance_list[[paste0("CMC-IMH Shard ", i)]] <-
      cmc_imh_post_burn_in_acceptance_rates[i]
  }

  acceptance_list[["Mean CMC-IMH"]] <-
    mean(cmc_imh_post_burn_in_acceptance_rates)
}

if (length(acceptance_tables) > 0) {
  acceptance_rates <- do.call(rbind, acceptance_tables)
  acceptance_rates$Summary <- FALSE

  acceptance_summary <- aggregate(
    Acceptance_Rate ~ Method,
    data = acceptance_rates,
    FUN = mean
  )
  acceptance_summary$Level <- "Mean"
  acceptance_summary$Shard <- NA_integer_
  acceptance_summary$Chain <- NA_integer_
  acceptance_summary$Summary <- TRUE
  acceptance_summary <- acceptance_summary[, names(acceptance_rates)]

  acceptance_rates <- rbind(acceptance_rates, acceptance_summary)

  safe_write_csv(
    acceptance_rates,
    "Results/acceptance_rates.csv"
  )
} else if (length(acceptance_list) > 0) {
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
} else if (exists("log_prior")) {
  settings <- rbind(
    settings,
    add_setting("Cauchy prior scale", 2.5)
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

if (exists("num_chains")) {
  settings <- rbind(
    settings,
    add_setting("Number of independent chains", num_chains)
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

# Full-data RWMH histogram and trace plots. In the multi-chain workflow,
# a representative chain is plotted because pooled samples are not one chain.
full_data_rwmh_plot_samples <- representative_samples(
  "full_data_rwmh_post_samples_list",
  "full_data_rwmh_post_samples",
  "full_data_rwmh_plot_chain_id"
)

if (!is.null(full_data_rwmh_plot_samples) && exists("hist_trace_plot")) {
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

  hist_trace_plot(full_data_rwmh_plot_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/full_data_rwmh_hist_trace_%02d.pdf",
    "Figures",
    "Full-data RWMH posterior histograms and trace plots"
  )
}

# Full-data RWMH ACF plots
if (!is.null(full_data_rwmh_plot_samples) && exists("acf_mcmc_plot")) {
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

  acf_mcmc_plot(full_data_rwmh_plot_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/full_data_rwmh_acf_%02d.pdf",
    "Figures",
    "Full-data RWMH posterior autocorrelation plots"
  )
}

# Full-data IMH histogram and trace plots
full_data_imh_plot_samples <- representative_samples(
  "full_data_imh_post_samples_list",
  "full_data_imh_post_samples",
  "full_data_imh_plot_chain_id"
)

if (!is.null(full_data_imh_plot_samples) && exists("hist_trace_plot")) {
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

  hist_trace_plot(full_data_imh_plot_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/full_data_imh_hist_trace_%02d.pdf",
    "Figures",
    "Full-data IMH posterior histograms and trace plots"
  )
}

# Full-data IMH ACF plots
if (!is.null(full_data_imh_plot_samples) && exists("acf_mcmc_plot")) {
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

  acf_mcmc_plot(full_data_imh_plot_samples, params_per_page = 2)

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

# CMC-RWMH representative final consensus chain histogram and trace plots
cmc_rwmh_plot_samples <- representative_samples(
  "cmc_rwmh_consensus_samples_list",
  "cmc_rwmh_samples",
  "cmc_rwmh_plot_chain_id"
)

if (!is.null(cmc_rwmh_plot_samples) && exists("hist_trace_plot")) {
  pdf(
    "Figure/cmc_rwmh_hist_trace_%02d.pdf",
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

  hist_trace_plot(cmc_rwmh_plot_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/cmc_rwmh_hist_trace_%02d.pdf",
    "Figures",
    "CMC-RWMH representative final consensus chain histograms and trace plots"
  )
}

if (!is.null(cmc_rwmh_plot_samples) && exists("acf_mcmc_plot")) {
  pdf(
    "Figure/cmc_rwmh_acf_%02d.pdf",
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

  acf_mcmc_plot(cmc_rwmh_plot_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/cmc_rwmh_acf_%02d.pdf",
    "Figures",
    "CMC-RWMH representative final consensus chain autocorrelation plots"
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

# CMC-IMH representative final consensus chain histogram and trace plots
cmc_imh_plot_samples <- representative_samples(
  "cmc_imh_consensus_samples_list",
  "cmc_imh_samples",
  "cmc_imh_plot_chain_id"
)

if (!is.null(cmc_imh_plot_samples) && exists("hist_trace_plot")) {
  pdf(
    "Figure/cmc_imh_hist_trace_%02d.pdf",
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

  hist_trace_plot(cmc_imh_plot_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/cmc_imh_hist_trace_%02d.pdf",
    "Figures",
    "CMC-IMH representative final consensus chain histograms and trace plots"
  )
}

if (!is.null(cmc_imh_plot_samples) && exists("acf_mcmc_plot")) {
  pdf(
    "Figure/cmc_imh_acf_%02d.pdf",
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

  acf_mcmc_plot(cmc_imh_plot_samples, params_per_page = 2)

  par(old_par)
  dev.off()

  register_artifact(
    "Figure/cmc_imh_acf_%02d.pdf",
    "Figures",
    "CMC-IMH representative final consensus chain autocorrelation plots"
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
  "num_chains",
  "full_data_rwmh_result",
  "full_data_imh_result",
  "full_data_rwmh_results",
  "full_data_imh_results",
  "full_data_rwmh_chain",
  "full_data_imh_chain",
  "full_data_rwmh_post_samples_list",
  "full_data_imh_post_samples_list",
  "full_data_rwmh_post_samples",
  "full_data_imh_post_samples",
  "full_data_rwmh_post_stats_table",
  "full_data_imh_post_stats_table",
  "full_data_rwmh_post_burn_in_acceptance_rates",
  "full_data_imh_post_burn_in_acceptance_rates",
  "full_data_rwmh_gelman_diag",
  "full_data_imh_gelman_diag",
  "full_data_rwmh_ess_by_parameter",
  "full_data_imh_ess_by_parameter",
  "full_data_rwmh_ess_table",
  "full_data_imh_ess_table",
  "cmc_rwmh_results",
  "cmc_imh_results",
  "cmc_rwmh_shard_post_samples_by_chain_list",
  "cmc_imh_shard_post_samples_by_chain_list",
  "cmc_rwmh_subposterior_samples_list",
  "cmc_imh_subposterior_samples_list",
  "cmc_rwmh_post_burn_in_acceptance_rates",
  "cmc_imh_post_burn_in_acceptance_rates",
  "cmc_rwmh_post_burn_in_acceptance_rates_by_shard",
  "cmc_imh_post_burn_in_acceptance_rates_by_shard",
  "cmc_rwmh_mean_post_burn_in_acceptance_rates",
  "cmc_imh_mean_post_burn_in_acceptance_rates",
  "cmc_rwmh_gelman_diag_list",
  "cmc_imh_gelman_diag_list",
  "cmc_rwmh_shard_ess_table",
  "cmc_imh_shard_ess_table",
  "cmc_rwmh_consensus_samples_list",
  "cmc_imh_consensus_samples_list",
  "cmc_rwmh_consensus_gelman_diag",
  "cmc_imh_consensus_gelman_diag",
  "cmc_rwmh_ess_by_parameter",
  "cmc_imh_ess_by_parameter",
  "cmc_rwmh_ess_table",
  "cmc_imh_ess_table",
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
