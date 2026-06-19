# Export results for thesis tables and figures
# Run this after Consensus_Monte_Carlo_multi_chain.r has completed or
# partially completed in the current R session.

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

expected_num_chains <- if (exists("num_chains", inherits = TRUE)) {
  as.integer(get("num_chains", inherits = TRUE))
} else {
  4L
}

params_per_diagnostic_page <- 2L

required_result_files <- c(
  "Results/model_settings.csv",
  "Results/response_distribution.csv",
  "Results/road_type_distribution.csv",
  "Results/speed_limit_distribution.csv",
  "Results/reference_categories.csv",
  "Results/frequentist_logistic_summary.csv",
  "Results/frequentist_likelihood_ratio_test.csv",
  "Results/frequentist_gvif.csv",
  "Results/pearson_residual_summary.csv",
  "Results/deviance_residual_summary.csv",
  "Results/cmc_shard_sizes.csv",
  "Results/cmc_shard_response_distribution.csv",
  "Results/full_data_rwmh_posterior_summary.csv",
  "Results/full_data_imh_posterior_summary.csv",
  "Results/cmc_rwmh_posterior_summary.csv",
  "Results/cmc_imh_posterior_summary.csv",
  "Results/posterior_mean_comparison.csv",
  "Results/cmc_rmse_comparison.csv",
  "Results/acceptance_rates.csv",
  "Results/runtime_comparison.csv",
  "Results/gelman_rubin_diagnostics.csv",
  "Results/effective_sample_size.csv",
  "Results/report_artifact_manifest.csv",
  "Results/export_object_availability.csv"
)

fixed_road_type_figures <- file.path(
  "Figure",
  c(
    "road_type_single_carriageway.png",
    "road_type_dual_carriageway.png",
    "road_type_roundabout.png",
    "road_type_slip_road.png"
  )
)

diagnostic_figure_prefixes <- c(
  "full_data_rwmh_hist_trace",
  "full_data_rwmh_acf",
  "full_data_imh_hist_trace",
  "full_data_imh_acf",
  "cmc_rwmh_hist_trace",
  "cmc_rwmh_acf",
  "cmc_imh_hist_trace",
  "cmc_imh_acf"
)

required_diagnostic_figures <- unlist(
  lapply(
    diagnostic_figure_prefixes,
    function(prefix) {
      file.path(
        "Figure",
        sprintf("%s_%02d.pdf", prefix, seq_len(5L))
      )
    }
  ),
  use.names = FALSE
)

required_figure_files <- c(
  fixed_road_type_figures,
  "Figure/residual_acf_correlograms.pdf",
  required_diagnostic_figures
)

known_supplementary_result_files <- c(
  "Results/data_preview.csv",
  "Results/parameter_names.csv",
  "Results/design_matrix_info.csv",
  "Results/frequentist_coefficient_covariance.csv",
  "Results/full_data_rwmh_chain_dimensions.csv",
  "Results/full_data_imh_chain_dimensions.csv",
  "Results/cmc_rwmh_chain_dimensions.csv",
  "Results/cmc_imh_chain_dimensions.csv",
  "Results/full_data_rwmh_chain_posterior_summary.csv",
  "Results/full_data_imh_chain_posterior_summary.csv",
  "Results/cmc_rwmh_chain_posterior_summary.csv",
  "Results/cmc_imh_chain_posterior_summary.csv",
  "Results/ess_mcse_comparison.csv",
  "Results/cmc_shard_gelman_rubin_diagnostics.csv",
  "Results/cmc_shard_effective_sample_size.csv",
  "Results/cmc_rwmh_posterior_mean_error.csv",
  "Results/cmc_imh_posterior_mean_error.csv",
  "Results/cmc_rwmh_rmse.csv",
  "Results/cmc_imh_rmse.csv",
  "Results/cmc_shard_road_type_distribution.csv",
  "Results/cmc_shard_speed_limit_distribution.csv",
  "Results/proposal_cov_full_rwmh.csv",
  "Results/proposal_cov_full_imh.csv",
  "Results/proposal_cov_cmc_rwmh.csv"
)

thesis_required_files <- c(required_result_files, required_figure_files)
exported_paths <- character()

report_artifact_manifest <- data.frame(
  Artifact = character(),
  File_Path = character(),
  Artifact_Type = character(),
  Report_Section = character(),
  Description = character(),
  Required_For_Thesis = logical(),
  Generated_By_Export = logical(),
  Exists = logical(),
  Notes = character(),
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

infer_artifact_type <- function(file_path) {
  extension <- tolower(tools::file_ext(file_path))

  if (extension %in% c("pdf", "png", "jpg", "jpeg", "svg")) {
    return("Figure")
  }

  if (extension == "csv") {
    return("Result table")
  }

  if (extension == "rds") {
    return("Supplementary data")
  }

  "Artifact"
}

register_artifact <- function(file_path,
                              report_section = NULL,
                              description = NULL,
                              required_for_thesis = file_path %in% thesis_required_files,
                              generated_by_export = file_path %in% exported_paths,
                              notes = "") {
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
        Artifact_Type = infer_artifact_type(file_path),
        Report_Section = report_section,
        Description = description,
        Required_For_Thesis = required_for_thesis,
        Generated_By_Export = generated_by_export,
        Exists = file.exists(file_path),
        Notes = notes,
        stringsAsFactors = FALSE
      )
    )
  )
}

safe_write_csv <- function(object, file_path, report_section = NULL, description = NULL) {
  if (!is.null(object)) {
    write.csv(object, file_path, row.names = FALSE)
    exported_paths <<- unique(c(exported_paths, file_path))
    register_artifact(file_path, report_section, description)
  }
}

safe_write_matrix <- function(object, file_path, report_section = NULL, description = NULL) {
  if (!is.null(object)) {
    write.csv(as.data.frame(object), file_path, row.names = TRUE)
    exported_paths <<- unique(c(exported_paths, file_path))
    register_artifact(file_path, report_section, description)
  }
}

safe_save_rds <- function(object, file_path, report_section = NULL, description = NULL) {
  if (!is.null(object)) {
    saveRDS(object, file_path)
    exported_paths <<- unique(c(exported_paths, file_path))
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
    !is.null(colnames(samples)) &&
    all(nzchar(colnames(samples))) &&
    all(is.finite(samples))
}

sample_list_problems <- function(samples_list,
                                 expected_chains = expected_num_chains) {
  problems <- character()

  if (!is.list(samples_list) || length(samples_list) == 0) {
    return("is not a non-empty list")
  }

  if (length(samples_list) < expected_chains) {
    problems <- c(
      problems,
      paste0(
        "contains ",
        length(samples_list),
        " chains; expected at least ",
        expected_chains
      )
    )
  }

  matrix_check <- vapply(
    samples_list,
    function(samples) {
      is.matrix(samples) && nrow(samples) > 0 && ncol(samples) > 0
    },
    logical(1)
  )

  if (!all(matrix_check)) {
    problems <- c(problems, "contains a non-matrix or empty chain")
    return(unique(problems))
  }

  nrows <- vapply(samples_list, nrow, integer(1))
  ncols <- vapply(samples_list, ncol, integer(1))

  if (length(unique(nrows)) != 1 || length(unique(ncols)) != 1) {
    problems <- c(problems, "has inconsistent chain dimensions")
  }

  if (any(!vapply(samples_list, function(x) all(is.finite(x)), logical(1)))) {
    problems <- c(problems, "contains non-finite sample values")
  }

  if (any(vapply(samples_list, function(x) is.null(colnames(x)), logical(1)))) {
    problems <- c(problems, "has missing parameter column names")
    return(unique(problems))
  }

  if (any(vapply(
    samples_list,
    function(x) any(!nzchar(colnames(x))),
    logical(1)
  ))) {
    problems <- c(problems, "has blank parameter column names")
  }

  colname_check <- vapply(
    samples_list,
    function(samples) {
      identical(colnames(samples), colnames(samples_list[[1]]))
    },
    logical(1)
  )

  if (!all(colname_check)) {
    problems <- c(problems, "has inconsistent parameter column names")
  }

  unique(problems)
}

valid_sample_list <- function(samples_list,
                              expected_chains = expected_num_chains) {
  length(sample_list_problems(samples_list, expected_chains)) == 0
}

ensure_valid_sample_list <- function(samples_list,
                                     object_name,
                                     expected_chains = expected_num_chains) {
  problems <- sample_list_problems(samples_list, expected_chains)

  if (length(problems) > 0) {
    warning(
      object_name,
      " ",
      paste(problems, collapse = "; "),
      ".",
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

representative_chain_samples <- function(samples_list_name,
                                         chain_id_name = NULL) {
  samples_list <- object_value(samples_list_name)

  if (!ensure_valid_sample_list(samples_list, samples_list_name)) {
    return(NULL)
  }

  chain_id <- 1L

  if (!is.null(chain_id_name) && object_exists(chain_id_name)) {
    candidate_id <- suppressWarnings(
      as.integer(object_value(chain_id_name)[1])
    )

    if (!is.na(candidate_id) &&
        candidate_id >= 1 &&
        candidate_id <= length(samples_list)) {
      chain_id <- candidate_id
    } else {
      warning(
        chain_id_name,
        " is invalid; representative chain 1 will be used.",
        call. = FALSE
      )
    }
  } else {
    warning(
      if (is.null(chain_id_name)) {
        paste0(
          "No plot-chain ID was supplied for ",
          samples_list_name,
          "; representative chain 1 will be used."
        )
      } else {
        paste0(
          chain_id_name,
          " is missing; representative chain 1 will be used."
        )
      },
      call. = FALSE
    )
  }

  samples_list[[chain_id]]
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

missing_fixed_road_type_figures <- fixed_road_type_figures[
  !file.exists(fixed_road_type_figures)
]

if (length(missing_fixed_road_type_figures) > 0) {
  warning(
    "The following fixed road-type figures required by Chapter 3 are missing: ",
    paste(missing_fixed_road_type_figures, collapse = ", "),
    call. = FALSE
  )
}

for (fixed_figure in fixed_road_type_figures) {
  register_artifact(
    fixed_figure,
    "Data and preprocessing",
    paste(
      "Fixed road-type example:",
      tools::toTitleCase(
        gsub(
          "_",
          " ",
          tools::file_path_sans_ext(basename(fixed_figure))
        )
      )
    ),
    required_for_thesis = TRUE,
    generated_by_export = FALSE,
    notes = if (file.exists(fixed_figure)) {
      "Fixed thesis image verified; not generated by Export_Results.r."
    } else {
      "Fixed thesis image is missing and was not generated."
    }
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
  } else {
    warning(
      sample_info$samples_list,
      " is missing; chain dimensions and chain-specific summaries were not exported.",
      call. = FALSE
    )
  }

  pooled_samples <- object_value(sample_info$pooled_samples)

  if (valid_sample_matrix(pooled_samples)) {
    safe_save_rds(
      pooled_samples,
      file.path("Results", paste0(file_prefix, "_pooled_post_samples.rds")),
      "Posterior summaries",
      paste(sample_info$description, "pooled posterior samples")
    )
  } else if (!is.null(pooled_samples)) {
    warning(
      sample_info$pooled_samples,
      " is invalid, non-finite, empty, or lacks parameter column names.",
      call. = FALSE
    )
  } else {
    warning(
      sample_info$pooled_samples,
      " is missing; pooled posterior samples were not exported.",
      call. = FALSE
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

fig_dpi <- 100

fig_width_px <- 800
fig_height_px <- 600
fig_width_in <- fig_width_px / fig_dpi
fig_height_in <- fig_height_px / fig_dpi

fig_wide_width_px <- 800
fig_wide_height_px <- 300
fig_wide_width_in <- fig_wide_width_px / fig_dpi
fig_wide_height_in <- fig_wide_height_px / fig_dpi

export_paginated_diagnostic <- function(samples,
                                        plot_function_name,
                                        file_prefix,
                                        description,
                                        width,
                                        height,
                                        par_settings) {
  if (is.null(samples)) {
    warning(
      "Cannot export ",
      file_prefix,
      " because no valid representative chain is available.",
      call. = FALSE
    )
    return(character())
  }

  if (!exists(plot_function_name, mode = "function", inherits = TRUE)) {
    warning(
      plot_function_name,
      " is missing; ",
      file_prefix,
      " figures were not generated.",
      call. = FALSE
    )
    return(character())
  }

  expected_pages <- ceiling(ncol(samples) / params_per_diagnostic_page)

  if (ncol(samples) == 10L && expected_pages != 5L) {
    warning(
      file_prefix,
      " should have 5 pages for 10 parameters and 2 parameters per page.",
      call. = FALSE
    )
  }

  temp_directory <- tempfile(paste0(file_prefix, "_"))
  dir.create(temp_directory)
  on.exit(unlink(temp_directory, recursive = TRUE, force = TRUE), add = TRUE)

  temp_pattern <- file.path(
    temp_directory,
    paste0(file_prefix, "_%02d.pdf")
  )

  device_open <- FALSE
  export_succeeded <- tryCatch(
    {
      pdf(
        temp_pattern,
        width = width,
        height = height,
        onefile = FALSE
      )
      device_open <- TRUE

      old_par <- par(no.readonly = TRUE)
      do.call(par, par_settings)

      do.call(
        get(plot_function_name, mode = "function", inherits = TRUE),
        list(
          samples = samples,
          params_per_page = params_per_diagnostic_page
        )
      )

      par(old_par)
      dev.off()
      device_open <- FALSE
      TRUE
    },
    error = function(error_condition) {
      if (device_open && dev.cur() > 1L) {
        dev.off()
      }

      warning(
        "Failed to generate ",
        file_prefix,
        ": ",
        conditionMessage(error_condition),
        call. = FALSE
      )
      FALSE
    }
  )

  if (!export_succeeded) {
    return(character())
  }

  generated_temp_files <- sort(
    list.files(
      temp_directory,
      pattern = paste0("^", file_prefix, "_[0-9]{2}\\.pdf$"),
      full.names = TRUE
    )
  )

  if (length(generated_temp_files) != expected_pages) {
    warning(
      file_prefix,
      " generated ",
      length(generated_temp_files),
      " pages; expected ",
      expected_pages,
      ".",
      call. = FALSE
    )
    return(character())
  }

  target_files <- file.path(
    "Figure",
    sprintf("%s_%02d.pdf", file_prefix, seq_len(expected_pages))
  )

  existing_files <- list.files(
    "Figure",
    pattern = paste0("^", file_prefix, "_[0-9]{2}\\.pdf$"),
    full.names = TRUE
  )

  if (length(existing_files) > 0) {
    unlink(existing_files)
  }

  copied <- file.copy(
    generated_temp_files,
    target_files,
    overwrite = TRUE
  )

  if (!all(copied)) {
    warning(
      "Not all ",
      file_prefix,
      " diagnostic pages could be copied into Figure/.",
      call. = FALSE
    )
  }

  copied_files <- target_files[copied & file.exists(target_files)]
  exported_paths <<- unique(c(exported_paths, copied_files))

  for (page_index in seq_along(copied_files)) {
    register_artifact(
      copied_files[[page_index]],
      "Figures",
      paste0(description, ", page ", page_index)
    )
  }

  if (ncol(samples) == 10L && length(copied_files) != 5L) {
    warning(
      file_prefix,
      " has ",
      length(copied_files),
      " final pages; 5 are required by the thesis.",
      call. = FALSE
    )
  }

  copied_files
}

diagnostic_method_map <- list(
  full_data_rwmh = list(
    samples_list = "full_data_rwmh_post_samples_list",
    plot_chain_id = "full_data_rwmh_plot_chain_id",
    label = "Full-data RWMH"
  ),
  full_data_imh = list(
    samples_list = "full_data_imh_post_samples_list",
    plot_chain_id = "full_data_imh_plot_chain_id",
    label = "Full-data IMH"
  ),
  cmc_rwmh = list(
    samples_list = "cmc_rwmh_consensus_samples_list",
    plot_chain_id = "cmc_rwmh_plot_chain_id",
    label = "CMC-RWMH final consensus"
  ),
  cmc_imh = list(
    samples_list = "cmc_imh_consensus_samples_list",
    plot_chain_id = "cmc_imh_plot_chain_id",
    label = "CMC-IMH final consensus"
  )
)

for (method_key in names(diagnostic_method_map)) {
  method_info <- diagnostic_method_map[[method_key]]
  plot_samples <- representative_chain_samples(
    method_info$samples_list,
    method_info$plot_chain_id
  )

  export_paginated_diagnostic(
    samples = plot_samples,
    plot_function_name = "hist_trace_plot",
    file_prefix = paste0(method_key, "_hist_trace"),
    description = paste(
      method_info$label,
      "posterior histograms and trace plots"
    ),
    width = fig_width_in,
    height = fig_height_in,
    par_settings = list(
      cex.main = 0.80,
      cex.lab = 0.85,
      cex.axis = 0.85
    )
  )

  export_paginated_diagnostic(
    samples = plot_samples,
    plot_function_name = "acf_mcmc_plot",
    file_prefix = paste0(method_key, "_acf"),
    description = paste(
      method_info$label,
      "posterior autocorrelation plots"
    ),
    width = fig_wide_width_in,
    height = fig_wide_height_in,
    par_settings = list(
      cex.main = 0.75,
      cex.lab = 0.80,
      cex.axis = 0.80
    )
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

  exported_paths <- unique(
    c(exported_paths, "Figure/residual_acf_correlograms.pdf")
  )
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
  "full_data_rwmh_plot_chain_id",
  "full_data_imh_plot_chain_id",
  "cmc_rwmh_plot_chain_id",
  "cmc_imh_plot_chain_id",
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

# -------------------------------------------------------------------
# 12. Required artifact checklist and final manifest
# -------------------------------------------------------------------

for (required_file in thesis_required_files) {
  if (!required_file %in% report_artifact_manifest$File_Path) {
    register_artifact(
      required_file,
      required_for_thesis = TRUE,
      generated_by_export = required_file %in% exported_paths,
      notes = if (file.exists(required_file)) {
        "Required artifact exists but was not registered earlier."
      } else {
        "Required artifact is missing."
      }
    )
  }
}

for (supplementary_file in known_supplementary_result_files) {
  if (file.exists(supplementary_file) &&
      !supplementary_file %in% report_artifact_manifest$File_Path) {
    register_artifact(
      supplementary_file,
      required_for_thesis = FALSE,
      generated_by_export = supplementary_file %in% exported_paths,
      notes = paste(
        "Optional supplementary result exists but was not generated",
        "during this export run."
      )
    )
  }
}

manifest_path <- "Results/report_artifact_manifest.csv"
checklist_path <- "Results/thesis_required_artifact_checklist.csv"

exported_paths <- unique(
  c(exported_paths, manifest_path, checklist_path)
)

register_artifact(
  manifest_path,
  "Artifact validation",
  "Manifest of report-ready and supplementary artifacts",
  required_for_thesis = TRUE,
  generated_by_export = TRUE,
  notes = "Generated by Export_Results.r."
)

register_artifact(
  checklist_path,
  "Artifact validation",
  "Checklist of artifacts required by the current thesis",
  required_for_thesis = FALSE,
  generated_by_export = TRUE,
  notes = "Operational validation file generated by Export_Results.r."
)

deduplicate_manifest <- function(manifest) {
  manifest <- manifest[!duplicated(manifest$File_Path, fromLast = TRUE), ]
  manifest <- manifest[order(
    !manifest$Required_For_Thesis,
    manifest$Artifact_Type,
    manifest$File_Path
  ), ]
  rownames(manifest) <- NULL
  manifest
}

report_artifact_manifest$Exists <- file.exists(
  report_artifact_manifest$File_Path
)
report_artifact_manifest$Generated_By_Export <-
  report_artifact_manifest$File_Path %in% exported_paths
report_artifact_manifest <- deduplicate_manifest(report_artifact_manifest)

write.csv(
  report_artifact_manifest,
  manifest_path,
  row.names = FALSE
)

thesis_required_artifact_checklist <- data.frame(
  Artifact_Type = vapply(
    thesis_required_files,
    infer_artifact_type,
    character(1)
  ),
  File_Path = thesis_required_files,
  Required_For_Thesis = TRUE,
  Exists = file.exists(thesis_required_files),
  Notes = vapply(
    thesis_required_files,
    function(file_path) {
      if (file_path %in% fixed_road_type_figures) {
        if (file.exists(file_path)) {
          return("Fixed figure verified; not generated by Export_Results.r.")
        }

        return("Fixed figure is missing.")
      }

      if (file_path %in% exported_paths) {
        return("Generated or written during this export run.")
      }

      if (file.exists(file_path)) {
        return(
          paste(
            "File exists from an earlier run but was not generated",
            "during this export run; verify the required source object."
          )
        )
      }

      "Required artifact is missing."
    },
    character(1)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  thesis_required_artifact_checklist,
  checklist_path,
  row.names = FALSE
)

report_artifact_manifest$Exists <- file.exists(
  report_artifact_manifest$File_Path
)
report_artifact_manifest <- deduplicate_manifest(report_artifact_manifest)

write.csv(
  report_artifact_manifest,
  manifest_path,
  row.names = FALSE
)

required_not_currently_exported <- setdiff(
  thesis_required_files,
  c(
    exported_paths,
    fixed_road_type_figures[file.exists(fixed_road_type_figures)]
  )
)

if (length(required_not_currently_exported) > 0) {
  warning(
    "The following thesis-required artifacts were not generated during this export run: ",
    paste(required_not_currently_exported, collapse = ", "),
    call. = FALSE
  )
}

missing_required_artifacts <- thesis_required_files[
  !file.exists(thesis_required_files)
]

if (length(missing_required_artifacts) > 0) {
  warning(
    "The following thesis-required artifacts are missing after export: ",
    paste(missing_required_artifacts, collapse = ", "),
    call. = FALSE
  )
}

cat(
  "Export complete. Results saved in Results/ and thesis-required figures ",
  "saved or verified in Figure/.\n",
  sep = ""
)
