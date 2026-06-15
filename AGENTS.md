# AGENTS.md

## Project context

This repository contains my undergraduate final year thesis project on **Consensus Monte Carlo using Metropolis--Hastings on distributed data**.

The thesis is written in LaTeX using the XMUM thesis class file:

* `Report/main.tex`
* `Report/xmumthesis.cls`

The previous student thesis should be used only as a **formatting and structure reference**. Do not copy its wording, results, figures, tables, code, or mathematical exposition.

## Main rule

Do not invent content.

If information is missing, write `TODO:` clearly instead of guessing. This applies especially to:

* citations
* numerical results
* dataset details
* variable definitions
* algorithm settings
* posterior estimates
* acceptance rates
* convergence diagnostics
* computation time
* conclusions

## Current thesis implementation

The current empirical implementation is based on the following setup. Treat `Script/Consensus_Monte_Carlo.r`, the exported files in `Results/`, and the generated figures in `Figure/` as the source of truth.

* Dataset: UK accident data from `Dataset/accident_2013.csv`.
* Variables used: `road_type`, `speed_limit`, and `accident_severity`.
* Data preprocessing:
  * missing rows are removed with `na.omit()`;
  * `accident_severity` is recoded into a binary response;
  * observations with `road_type == "unknown"` are removed;
  * factor reference categories are set after removing the unknown road type.
* Response construction:
  * event coded as 1: `serious_fatal`, formed by combining serious and fatal accidents;
  * reference response class coded as 0: `slight`.
* Model: Bayesian logistic regression.
* Predictors: categorical `road_type` and categorical `speed_limit`.
* Reference category for `road_type`: `single carriageway`.
* Reference category for `speed_limit`: `2`.
* Prior: independent Cauchy priors for regression coefficients, with location 0 and scale 2.5.
* Full-data samplers:
  * full-data Random Walk Metropolis--Hastings (RWMH);
  * full-data Independent Metropolis--Hastings (IMH).
* Consensus Monte Carlo samplers:
  * subset or shard RWMH;
  * subset or shard IMH.
* Current number of shards: `K = 10`, as set by `cmc_num_shards <- 10`, unless later changed in `Script/Consensus_Monte_Carlo.r` and reflected in `Results/model_settings.csv`.
* Current MCMC settings from the script and exported model settings:
  * full-data MCMC iterations: 100000;
  * burn-in fraction: 0.2;
  * full-data burn-in iterations: 20000;
  * CMC shard MCMC iterations: 100000;
  * CMC shard burn-in iterations: 20000;
  * full-data RWMH tuning factor: 0.75;
  * full-data IMH tuning factor: 1.5;
  * CMC-RWMH tuning factor: 0.75;
  * CMC-IMH tuning factor: 1.5.
* Data partitioning for CMC: stratified random assignment by binary response class, so each shard receives a roughly balanced response-class allocation.
* Subposterior construction: each shard uses the shard likelihood and `1 / K` of the prior contribution, so that the product of subposteriors corresponds to the full-posterior structure.
* Consensus combination: precision-weighted combination of subposterior samples, using estimated shard covariance matrices and their inverse precision matrices.
* Runtime:
  * full-data RWMH and full-data IMH are run sequentially;
  * CMC-RWMH and CMC-IMH use `parallel::makeCluster()`, `clusterSetRNGStream()`, and `parallel::parLapply()` with the number of workers set to `cmc_num_shards`;
  * runtime values include the recorded execution time and any cluster overhead;
  * do not claim universal or hardware-independent speedup;
  * only claim speedup if the exported runtime results support it, and qualify that the runtime depends on the current machine and parallel backend.

Do not describe the current implementation as a purely sequential CMC implementation. Do not describe the current prior as normal unless the R script is changed accordingly.

## Project structure

Use the following folder structure:

* `Dataset/`: raw dataset and variable documentation
* `Figure/`: generated figures for the thesis
* `Report/`: LaTeX report files
* `Results/`: exported numerical results from the R analysis
* `Script/`: R scripts for data analysis, simulation, and export

Important files:

* `Report/main.tex`: main LaTeX file
* `Report/Chapter1_Introduction.tex`: Chapter 1
* `Report/Chapter2_LiteratureReview.tex`: Chapter 2
* `Report/Chapter3_Methodology.tex`: Chapter 3
* `Report/Chapter4_Results.tex`: Chapter 4
* `Report/Chapter5_Conclusion.tex`: Chapter 5
* `Report/Appendix.tex`: Appendix
* `Report/refs.bib`: bibliography
* `Script/Consensus_Monte_Carlo.r`: main R analysis script
* `Script/Export_Results.r`: result and figure export script
* `Dataset/accident_2013.csv`: dataset
* `Dataset/Variables for UK data.pdf`: variable documentation
* `Results/report_artifact_manifest.csv`: manifest of exported report-ready tables and figures, when available

## LaTeX file inclusion rule

In `Report/main.tex`, chapter files should be included using the actual filenames in the `Report/` folder:

```latex
\include{Chapter1_Introduction}
\include{Chapter2_LiteratureReview}
\include{Chapter3_Methodology}
\include{Chapter4_Results}
\include{Chapter5_Conclusion}

\appendix
\include{Appendix}
```

Do not use missing template filenames such as:

```latex
\include{Chapter1}
\include{Chapter2}
\include{Chapter3}
\include{appendix}
\include{AppendixA}
```

unless those files actually exist.

## Thesis structure

The thesis should follow this structure:

1. Front matter

   * Cover page
   * Declaration
   * Approval for submission
   * Copyright statement
   * Acknowledgements
   * Abstract
   * Table of contents
   * List of tables
   * List of figures
   * List of symbols and abbreviations

2. Chapter 1: Introduction

   * Background study
   * Problem statement
   * Research objectives
   * Research framework

3. Chapter 2: Literature Review

   * Bayesian logistic regression and prior specification
   * Markov Chain Monte Carlo
   * Metropolis--Hastings algorithm
   * Distributed Bayesian computation
   * Consensus Monte Carlo
   * Relevant applications or methodological comparisons

4. Chapter 3: Methodology

   * Data source
   * Data preprocessing
   * Bayesian logistic regression model
   * Prior distribution
   * Posterior distribution
   * Full-data Metropolis--Hastings methods
   * Consensus Monte Carlo methods
   * Posterior combination method
   * Posterior summaries and evaluation criteria

5. Chapter 4: Results and Analysis

   * Data summary
   * Frequentist logistic regression results
   * Full-data Metropolis--Hastings results
   * Consensus Monte Carlo results
   * Comparison of posterior estimates
   * Acceptance rates and computational runtime
   * Diagnostic plots
   * Summary of findings

6. Chapter 5: Conclusion

   * Summary of the project
   * Main findings
   * Limitations
   * Future work

7. Appendix

   * R code or code summary
   * additional tables
   * additional figures
   * variable information
   * supplementary diagnostics

8. References

## Writing style

Use formal academic writing.

Write in clear mathematical language. Avoid casual wording.

Avoid workflow-style phrases in the thesis, such as:

* “the code is unfinished”
* “terminal output”
* “saved output files in the Results folder”
* “if runtime output is available”
* “final R script”

Use thesis-style phrases instead, such as:

* “the empirical analysis”
* “the computational output”
* “the reported results”
* “the runtime values recorded during the analysis”

Use proper notation consistently:

* $\beta$: logistic regression coefficient vector
* $\beta_j$: the $j$th logistic regression coefficient
* $y$: binary response variable
* $X$: design matrix or predictor matrix
* $p_i$: probability that observation $i$ has serious/fatal accident severity
* $K$: number of data shards or subsets
* $m$: shard or subset index
* $\beta_m^{(s)}$: the $s$th subposterior sample from shard $m$
* $\Sigma_m$: estimated covariance matrix of shard $m$ subposterior samples
* $W_m = \Sigma_m^{-1}$: estimated precision matrix for shard $m$
* $\pi(\beta)$: prior distribution or prior density of $\beta$
* $L(y \mid \beta)$: likelihood function
* $p(\beta \mid y)$: posterior distribution of $\beta$ given $y$
* $p_m(\beta \mid y_m)$: subposterior distribution for shard $m$
* Use $\pi(\cdot)$ notation for priors, $L(\cdot)$ notation for likelihoods, and $p(\cdot \mid \cdot)$ notation for posterior and subposterior distributions.
* Do not include design matrix notation as a conditioning argument in likelihood, posterior, or subposterior notation. The predictor information may be defined in the model statement through $x_i^T\beta$.
* Use `$P$-value`, not `p-value`.
* Use `$z$-value` when referring to the standard normal test statistic.
* Use `$G^2$` when referring to the likelihood-ratio test statistic.
* Use $\mathcal{N}_d$ for a $d$-dimensional multivariate normal distribution. If the dimension is already known or clear from context, use $\mathcal{N}$ only.

Use the term **credible interval** for Bayesian posterior intervals, not confidence interval, unless discussing frequentist methods.

Use `Metropolis--Hastings`, not `Metropolis-Hastings`.

Use `serious/fatal accident severity` or `serious/fatal accidents` consistently when referring to the binary event. Do not write that slight accidents are the event being modelled.

Prefer `shard` when describing the current R implementation because the script uses `cmc_num_shards`, `cmc_shard_id`, and shard-specific objects. `Subset` is acceptable in general methodological explanation, but do not mix the two terms inconsistently within the same section.

## Citation rules

Do not create fake citations.

Only use citation keys that exist in `Report/refs.bib`.

If a citation is needed but unavailable, write:

```latex
TODO: add citation for this claim.
```

Do not invent author names, paper titles, publication years, journals, publishers, page numbers, or DOI numbers.

When adding a citation, check that the BibTeX entry exists in `Report/refs.bib`.

Do not remove TODO citation markers unless the claim is either cited properly or rewritten so that the citation is no longer needed.

## Mathematical content rules

Use LaTeX notation for equations.

Define all important terms before using them.

For Bayesian logistic regression, use notation consistent with:

```latex
\[
y_i \mid \beta \sim \operatorname{Bernoulli}(p_i),
\]
\[
\operatorname{logit}(p_i)=x_i^T\beta.
\]
```

For the Cauchy prior currently used in the R script, write:

```latex
\[
\beta_j \sim \operatorname{Cauchy}(0, 2.5), \qquad j=1,\ldots,d.
\]
```

For the posterior distribution, write:

```latex
\[
p(\beta \mid y) \propto L(y \mid \beta)\pi(\beta).
\]
```

For Consensus Monte Carlo, clearly distinguish between:

* the full posterior
* the shard likelihood
* the subposterior
* the combined consensus posterior

The subposterior should be written as:

```latex
\[
p_m(\beta \mid y_m)
\propto
L(y_m \mid \beta)\pi(\beta)^{1/K}.
\]
```

The precision-weighted consensus combination should be written as:

```latex
\[
\beta_{\mathrm{consensus}}^{(s)}
=
\left(\sum_{m=1}^{K} W_m\right)^{-1}
\sum_{m=1}^{K} W_m \beta_m^{(s)}.
\]
```

Do not claim that Consensus Monte Carlo is better unless the results support it.

Do not claim that the consensus samples are generated by a single Markov chain. The consensus samples are produced by combining post-burn-in subposterior samples from shard-specific Markov chains.

## R script rules

Do not overwrite the original dataset.

When editing R code:

* use clear variable names
* set a random seed for reproducibility
* save generated figures into `Figure/`
* save important numerical summaries into `Results/`
* avoid hard-coded absolute paths
* use relative paths from the project root

Preferred output folders:

* figures: `Figure/`
* tables or CSV summaries: `Results/`

If a folder does not exist, create it before saving output.

Do not modify `Script/Consensus_Monte_Carlo.r` unless explicitly instructed.

Do not rerun MCMC when only editing thesis text.

If the R script is edited, rerun `Script/Export_Results.r` after the main script completes so that `Results/` and `Figure/` remain synchronized with the implementation.

## Exported results rules

Chapter 4 must be based only on exported results and figures, not on memory or guessed values.

Prefer `Results/report_artifact_manifest.csv` when available because it records the exported report-ready artifacts and their intended report sections.

Use these files when available:

* `Results/model_settings.csv`
* `Results/data_preview.csv`
* `Results/response_distribution.csv`
* `Results/road_type_distribution.csv`
* `Results/speed_limit_distribution.csv`
* `Results/reference_categories.csv`
* `Results/frequentist_logistic_summary.csv`
* `Results/frequentist_likelihood_ratio_test.csv`
* `Results/frequentist_gvif.csv`
* `Results/pearson_residual_summary.csv`
* `Results/deviance_residual_summary.csv`
* `Results/frequentist_coefficient_covariance.csv`
* `Results/parameter_names.csv`
* `Results/design_matrix_info.csv`
* `Results/full_data_rwmh_posterior_summary.csv`
* `Results/full_data_imh_posterior_summary.csv`
* `Results/cmc_rwmh_posterior_summary.csv`
* `Results/cmc_imh_posterior_summary.csv`
* `Results/posterior_mean_comparison.csv`
* `Results/ess_mcse_comparison.csv`
* `Results/cmc_rwmh_rmse.csv`
* `Results/cmc_imh_rmse.csv`
* `Results/cmc_rmse_comparison.csv`
* `Results/cmc_rwmh_posterior_mean_error.csv`
* `Results/cmc_imh_posterior_mean_error.csv`
* `Results/acceptance_rates.csv`
* `Results/runtime_comparison.csv`
* `Results/cmc_shard_sizes.csv`
* `Results/cmc_shard_response_distribution.csv`
* `Results/cmc_shard_road_type_distribution.csv`
* `Results/cmc_shard_speed_limit_distribution.csv`
* `Results/proposal_cov_full_rwmh.csv`
* `Results/proposal_cov_full_imh.csv`
* `Results/proposal_cov_cmc_rwmh.csv`
* `Results/export_object_availability.csv`
* `Results/report_artifact_manifest.csv`

Use these figures when available:

* `Figure/full_data_rwmh_hist_trace_*.pdf`
* `Figure/full_data_rwmh_acf_*.pdf`
* `Figure/full_data_imh_hist_trace_*.pdf`
* `Figure/full_data_imh_acf_*.pdf`
* `Figure/cmc_rwmh_density_*.pdf`
* `Figure/cmc_imh_density_*.pdf`
* `Figure/residual_acf_correlograms.pdf`

The export script uses filename patterns such as `Figure/full_data_rwmh_hist_trace_%02d.pdf`. In the thesis, reference the actual generated numbered files, such as `_01.pdf`, `_02.pdf`, and so on, after verifying that the files exist.

For each MCMC or Consensus Monte Carlo result, report relevant diagnostics where available:

* posterior mean
* posterior median
* posterior standard deviation
* credible interval
* odds ratio
* effective sample size
* Monte Carlo standard error
* trace plot
* autocorrelation plot
* posterior density plot
* acceptance rate
* runtime
* convergence behaviour
* posterior mean RMSE for CMC methods, when available
* parameter-wise posterior mean error for CMC methods, when available

If diagnostics are not available, insert:

```latex
TODO: compute diagnostic.
```

Do not claim true parallel or computational speedup unless the exported runtime results support the claim. Because CMC-RWMH and CMC-IMH currently use parallel workers, any runtime discussion should state that the reported CMC runtime reflects the current parallel implementation and includes cluster setup and worker overhead.

If runtime values are based on sequential execution for a method, state that the reported runtime reflects the current sequential implementation of that method.

If CMC-IMH acceptance rates are low, discuss this cautiously as a possible tuning limitation. Do not treat low-acceptance CMC-IMH results as definitive without qualification.

## Chapter editing priorities

Current priority: rewrite and refine Chapters 1 to 4 before drafting Chapter 5.

Recommended order:

1. Refine Chapter 1 so that the introduction matches the current model, event definition, prior, shard count, and thesis scope.
2. Refine Chapter 3 so that the methodology exactly matches `Script/Consensus_Monte_Carlo.r`.
3. Refine Chapter 4 so that every numerical statement is supported by exported files in `Results/` and figures in `Figure/`.
4. Refine Chapter 2 after Chapters 1, 3, and 4 are stable, adding missing citations if needed.
5. Draft Chapter 5 only after Chapters 1 to 4 are consistent.

## Editing rules

Before rewriting a chapter, inspect the current file first.

Do not remove useful existing LaTeX structure unless necessary.

Do not rewrite the whole thesis unless specifically asked.

Prefer small, reviewable edits.

After modifying LaTeX files, check for:

* missing `\include{}` files
* missing citation keys
* missing figures
* missing tables
* syntax errors
* inconsistent notation
* unresolved TODOs
* claims unsupported by the exported results
* outdated references to normal priors, `K = 4`, purely sequential CMC, or old result filenames

When editing Chapter 4, verify that table values match the corresponding exported CSV files.

When editing Chapter 1 or Chapter 3, do not include Chapter 4 numerical results unless the section explicitly requires a high-level reference to evaluation criteria.

When editing Chapter 2, do not add papers unless their BibTeX entries are available or explicitly provided.