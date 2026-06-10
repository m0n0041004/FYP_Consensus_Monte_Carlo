# AGENTS.md

## Project context

This repository contains my undergraduate final year thesis project on **Consensus Monte Carlo**.

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

## Project structure

Use the following folder structure:

* `Dataset/`: raw dataset and variable documentation
* `Figure/`: generated figures for the thesis
* `Report/`: LaTeX report files
* `Script/`: R scripts for data analysis and simulation

Important files:

* `Report/main.tex`: main LaTeX file
* `Report/Chapter1_Introduction.tex`: Chapter 1
* `Report/Chapter2_LiteratureReview.tex`: Chapter 2
* `Report/Chapter3_Methodology.tex`: Chapter 3
* `Report/Chapter4_Results.tex`: Chapter 4
* `Report/Chapter5_Conclusion.tex`: Chapter 5
* `Report/AppendixA.tex`: Appendix
* `Report/refs.bib`: bibliography
* `Script/Consensus_Monte_Carlo.r`: main R script
* `Dataset/accident_2013.csv`: dataset
* `Dataset/Variables for UK data.pdf`: variable documentation

## LaTeX file inclusion rule

In `Report/main.tex`, chapter files should be included using the actual filenames:

```latex
\include{Chapter1_Introduction}
\include{Chapter2_LiteratureReview}
\include{Chapter3_Methodology}
\include{Chapter4_Results}
\include{Chapter5_Conclusion}

\appendix
\include{AppendixA}
```

Do not use missing template filenames such as:

```latex
\include{Chapter1}
\include{Chapter2}
\include{Chapter3}
\include{appendix}
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

   * Bayesian inference
   * Markov Chain Monte Carlo
   * Metropolis--Hastings algorithm
   * Distributed Bayesian computation
   * Consensus Monte Carlo
   * Relevant applications or comparisons

4. Chapter 3: Methodology

   * Data source
   * Data preprocessing
   * Bayesian model specification
   * Full-data MCMC method
   * Consensus Monte Carlo method
   * Posterior combination method
   * Algorithm settings
   * Evaluation criteria

5. Chapter 4: Results and Analysis

   * Descriptive statistics
   * Model setup
   * Full-data MCMC results
   * Consensus Monte Carlo results
   * Comparison of posterior estimates
   * Trace plots and posterior density plots
   * Convergence diagnostics
   * Runtime or computational efficiency comparison

6. Chapter 5: Conclusion

   * Summary of the project
   * Main findings
   * Limitations
   * Future work

7. Appendix

   * R code
   * additional tables
   * additional figures
   * variable information
   * supplementary diagnostics

8. References

## Writing style

Use formal academic writing.

Write in clear mathematical language. Avoid casual wording.

Use proper notation consistently. Suggested notation:

* $\theta$: parameter vector
* $y$: response variable
* $X$: design matrix or predictor matrix
* $\pi(\theta \mid y)$: posterior distribution
* $K$: number of data subsets
* $m$: subset index
* $\theta_m$: subposterior sample or subset-specific parameter draw

Use the term **credible interval** for Bayesian posterior intervals, not confidence interval, unless discussing frequentist methods.

## Citation rules

Do not create fake citations.

Only use citation keys that exist in `Report/refs.bib`.

If a citation is needed but unavailable, write:

```latex
TODO: add citation for this claim.
```

Do not invent author names, paper titles, publication years, journals, or DOI numbers.

When adding a citation, check that the BibTeX entry exists in `refs.bib`.

## Mathematical content rules

Use LaTeX notation for equations.

Define all important terms before using them.

For Bayesian inference, express the posterior generally as:

```latex
\[
\pi(\theta \mid y) \propto L(y \mid \theta)\pi(\theta).
\]
```

For Consensus Monte Carlo, clearly distinguish between:

* the full posterior
* the subposterior
* the combined consensus posterior

Do not claim that Consensus Monte Carlo is better unless the results support it.

## R script rules

Do not overwrite the original dataset.

When editing R code:

* use clear variable names
* set a random seed for reproducibility
* save generated figures into `Figure/`
* save important numerical summaries into a results table if possible
* avoid hard-coded absolute paths
* use relative paths from the project root

Preferred output folders:

* figures: `Figure/`
* tables or CSV summaries: `Report/tables/` or `Results/`

If a folder does not exist, create it before saving output.

## Results rules

Do not manually invent results for Chapter 4.

Results must come from the R script or provided output.

For each MCMC or Consensus Monte Carlo result, report relevant diagnostics where available:

* posterior mean
* posterior median
* posterior standard deviation
* credible interval
* trace plot
* posterior density plot
* acceptance rate
* runtime
* convergence behavior

If diagnostics are not available, insert:

```latex
TODO: compute diagnostic.
```

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

## Current priority

The current priority is to turn the template into a real first thesis draft.

Focus first on:

1. fixing `main.tex` file inclusion;
2. replacing template content in Chapter 1;
3. replacing template content in Chapter 3;
4. checking whether Chapter 4 exists and contains actual results;
5. expanding the bibliography;
6. adding real figures and tables generated from the R script.

Do not draft the conclusion until Chapter 4 results are available.
