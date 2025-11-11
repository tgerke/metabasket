# metabasket 0.0.0.9000

## Initial Development Release

### Core Features

* Added `basket_design()` function for creating basket trial specifications
* Added `basket_data()` function for creating trial data objects
* Implemented simulation framework with `simulate_responses()` and `simulate_basket_trial()`
* Created unified `analyze_basket()` interface with method dispatch
* Added `compute_operating_characteristics()` for evaluating design performance
* Implemented `generate_protocol_language()` for creating protocol text

### Parallel Computing & Progress Reporting

* **NEW**: Added optional parallelization support via the future framework
  - New `.parallelize` parameter in `simulate_basket_trial()` (default: `FALSE` for backward compatibility)
  - Uses `furrr::future_map()` for parallel simulation execution
  - Respects user's `future::plan()` settings for flexible backend configuration
  - Supports both single-stage and two-stage (Cunanan) trial simulations
  - Tested on all designs (BMA, MEM, BHM, Cunanan)
  - Achieves 1.5-2.5x speedup with multisession workers
  - See vignettes for usage examples
* **NEW**: Added progress reporting via the progressr package
  - Built-in progress updates for all simulations (silent by default)
  - Users control if/when/how progress is displayed via `progressr::handlers()`
  - Works seamlessly with both sequential and parallel execution
  - Near-live progress updates with supported future backends (multisession, multicore, cluster)
  - Example: `progressr::handlers("progress"); simulate_basket_trial(...)`
  - See progressr package documentation for customization options

### Statistical Methods

* **Cunanan et al. (2017) Efficient Design** - Fully validated native implementation:
  - Two-stage adaptive approach with interim heterogeneity test
  - Fisher's exact test for assessing homogeneity
  - Heterogeneous path (separate basket analysis) or homogeneous path (pooled analysis)
  - Complete test suite with validation against published operating characteristics
* **Simon Two-Stage Design** - Fully validated reference comparator:
  - Parallel independent analyses (no information borrowing)
  - Optimal or minimax design selection via clinfun
  - Bonferroni correction for family-wise error rate control
  - Two-stage futility stopping per basket
  - Fallback calculation when clinfun unavailable
* **Bayesian Model Averaging (BMA)** - Validated wrapper for bmabasket package:
  - Exchangeable and stratified priors
  - Cluster probability analysis
  - Hypothesis testing with posterior probabilities
* **Multi-source Exchangeability Model (MEM)** - Validated wrapper for basket package:
  - Information borrowing via JSD measure
  - Hierarchical Bayesian framework
  - Configurable prior parameters (shape1, shape2, tau)
* **Bayesian Hierarchical Model (BHM)** - Validated wrapper for bhmbasket package:
  - Multiple methods: Berry, EXNEX, pooled, stratified
  - MCMC-based posterior estimation via JAGS
  - Configurable evidence levels and iteration counts
  - doFuture integration for foreach compatibility

### Testing Infrastructure

* Comprehensive test suite with 375 passing tests across 9 test files
* Full validation for all five design types (BMA, MEM, BHM, Cunanan, Simon)
* Tests for basket_design validation and configuration
* Tests for basket_data with published trial data (Imatinib, Vemurafenib)
* Tests for simulation functions with reproducibility checks
* Tests for protocol language generation
* Extensive tests for Cunanan design (interim analysis, futility stopping, final analyses)
* Extensive tests for Simon design (parameter calculation, FWER control, power)
* **NEW**: 12 parallel execution tests verifying identical results between sequential and parallel modes
* doFuture backend registration in test setup to suppress foreach messages
* Tests gracefully skip when optional method packages (bmabasket, basket, bhmbasket) are not available
* CI workflow removes packages with platform-specific compilation issues before testing (macOS ARM64)

### Documentation

* Comprehensive README with quick start guide and installation instructions
* Introduction vignette covering all five design types
* **NEW**: Drug combination trial vignette with real-world simulation examples
* **NEW**: Design comparison vignette demonstrating method selection
* **NEW**: Simon designs vignette showcasing parallel workflows and protocol generation
* Set up pkgdown website configuration with logo
* All functions fully documented with examples
* Utility functions for design comparison and sample size calculation

### Example Data

* Imatinib trial data (Chugh et al. 2009) - `.rda` format in `data/`
* Vemurafenib trial data (Hyman et al. 2015) - `.rda` format in `data/`
* Example dataset creation scripts in `data-raw/` using `usethis::use_data()`

### Vignette Optimizations

* **NEW**: Reduced simulation counts in vignettes (1000 → 250) for faster builds
* **NEW**: Enabled parallel processing in vignettes with `plan(multisession, workers = availableCores() - 1)`
* **NEW**: Vignette build time reduced from >60 minutes to ~15-20 minutes (~75% improvement)
* **NEW**: Added explanatory notes for MEM FWER behavior in heterogeneous scenarios
* **NEW**: Fixed NA display in tables (now shows "N/A (no alternative)" / "N/A (no null)")

### Package Structure

* Follows R Packages (2e) best practices
* Uses base pipe `|>` throughout (no magrittr dependency)
* LazyData enabled for example datasets
* Clean separation: `data/` (binary .rda), `data-raw/` (generation scripts), `R/` (documentation)
* Future and furrr in Imports for core parallel functionality
* Method-specific packages (bmabasket, basket, bhmbasket) in Suggests
* doFuture in Suggests for foreach backend compatibility

### Known Limitations

* Bayesian methods (BMA, MEM, BHM) require external packages and dependencies
* BHM requires JAGS installation for MCMC sampling
* Parallel execution performance depends on the number of available cores
* Very large simulation studies (n > 10,000) may require significant memory

### Enhanced Reporting and Documentation (NEW)

* **Analysis reporting with confidence intervals**:
  - New `generate_analysis_report()` function creates comprehensive analysis reports
  - Includes exact binomial confidence intervals for response rates
  - Supports multiple output formats: text, markdown, HTML, LaTeX
  - Stage-specific reporting for two-stage designs (Cunanan, Simon)
  - Decision summaries with design-specific details
* **Multiple export formats for protocol documents**:
  - Enhanced `export_protocol_language()` supports .md, .html, .tex, .docx, .Rmd formats
  - R Markdown export includes reproducible code chunks
  - HTML export with styled output for web viewing
  - LaTeX export for formal protocol documents
  - Word export (.docx) via officer package for easy sharing and editing
  - Format automatically determined from file extension
* **Conditional power calculations for adaptive decisions**:
  - New `calculate_conditional_power()` function for interim decision support
  - Monte Carlo-based conditional power estimation
  - Supports both Cunanan and Simon designs
  - Helps determine whether to continue baskets based on interim data
* **Pre-registration document generation**:
  - New `generate_preregistration()` function creates structured pre-registration documents
  - Includes study design, hypotheses, and statistical analysis plan
  - Suitable for submission to OSF, ClinicalTrials.gov, etc.
  - Promotes transparency and rigor in trial conduct

### Future Enhancements

* Additional design types as new methods are published
* Extended reporting features:
  - Adjusted p-values and median unbiased estimates for sequential designs
  - Comprehensive stage-by-stage summary tables (cumulative statistics, boundaries, power)
  - Automated sensitivity analysis reports
  - Design comparison reports with visualization
* Additional real-world trial examples
* Shiny app for interactive design exploration
