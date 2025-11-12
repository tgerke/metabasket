# Simon Two-Stage Designs in metabasket

``` r
library(metabasket)
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

## Introduction

Simon two-stage designs (Simon 1989) are the gold standard for
single-arm phase II trials. The
[`clinfun::ph2simon`](https://rdrr.io/pkg/clinfun/man/ph2simon.html)
function provides highly accurate and computationally efficient
calculations of optimal and minimax designs. However, when conducting
simulation studies or designing basket trials with independent Simon
designs per basket, researchers face two common challenges:

1.  **Parallelization**: Running thousands of simulations can be
    time-consuming
2.  **Documentation**: Translating design parameters into clear,
    shareable protocol language

The `metabasket` package addresses both limitations while leveraging
[`clinfun::ph2simon`](https://rdrr.io/pkg/clinfun/man/ph2simon.html) for
the core design calculations.

## Advantages of metabasket’s Simon Implementation

### 1. Parallel Simulation Infrastructure

`metabasket` integrates Simon designs with its parallel simulation
framework, enabling:

- Automatic parallelization via `future` backend
- Progress reporting with `progressr`
- Consistent interface with other basket trial designs
- Operating characteristics calculated across parallel workers

### 2. Automated Protocol Language Generation

Generate publication-ready protocol text and export to multiple formats:

- Markdown (`.md`)
- HTML (`.html`)
- LaTeX (`.tex`)
- Word (`.docx`)
- R Markdown (`.Rmd`)

This bridges the gap between statistical design and regulatory/clinical
communication.

## Basic Simon Design Example

### Single Basket (Traditional Use Case)

Create a Simon optimal design for a single basket:

``` r
# Design a Simon optimal two-stage trial
# H0: p <= 0.20 vs H1: p >= 0.40
# Alpha = 0.10, Power = 0.80
design <- simon_design(
  n_baskets = 1,
  basket_names = "NSCLC",
  sample_sizes = 43,  # Maximum sample size
  null_response_rates = 0.20,
  alternative_response_rates = 0.40,
  alpha = 0.10,
  beta = 0.20,
  design_type = "optimal"
)

print(design)
#> Simon Two-Stage Design (Parallel Independent Designs)
#> =====================================================
#> 
#> Design Type: OPTIMAL 
#>   - optimal: minimizes expected sample size under H0
#>   - minimax: minimizes maximum sample size
#> 
#> Number of cohorts: 1 
#> Per-cohort error rates:
#>   - Alpha (type I): 0.1 
#>   - Beta (type II): 0.2 
#>   - Power: 0.8 
#> 
#> Cohort specifications:
#>   NSCLC: n=43, p0=0.200
#> 
#> Note: No information borrowing between cohorts.
#> For FWER control at level epsilon, use alpha = epsilon / n_baskets (Bonferroni).
```

The design parameters come directly from
[`clinfun::ph2simon`](https://rdrr.io/pkg/clinfun/man/ph2simon.html),
ensuring accuracy.

### Generate Protocol Language

Convert the design to human-readable protocol text suitable for
regulatory submissions:

``` r
protocol_text <- generate_protocol_language(design)
cat(protocol_text)
#> # Simon Two-Stage Design
#> 
#> ## Trial Overview
#> 
#> This trial uses a Simon two-stage design, the gold standard approach for single-arm phase II trials. The OPTIMAL design has been selected to minimize the expected sample size under the null hypothesis while maintaining the specified Type I error rate and power.
#> 
#> ## Trial Population
#> 
#> NSCLC (Maximum N = 43 patients)
#> 
#> The maximum sample size for this trial is 43 patients. The trial uses a Simon two-stage design optimized for single-arm phase II trials.
#> 
#> ## Statistical Methodology
#> 
#> This trial uses a Simon two-stage design, the most widely accepted approach for single-arm phase II trials in oncology. The design allows for early stopping if the treatment shows insufficient activity, thereby minimizing patient exposure to ineffective therapy while maintaining appropriate statistical properties.
#> 
#> ### Two-Stage Design
#> 
#> Each cohort follows a two-stage design:
#> 
#> **Stage 1**: Enroll n1 patients and observe the number of responses r1.
#> - If responses <= r1, stop for futility (cohort is not promising)
#> - If responses > r1, continue to Stage 2
#> 
#> **Stage 2**: Enroll additional patients to reach total sample size n.
#> - At final analysis, if total responses > r, reject H0 (cohort is promising)
#> - Otherwise, fail to reject H0
#> 
#> The design parameters (n1, r1, n, r) are calculated using the Simon (1989) method with the following specifications:
#> - Null response rate (H0): 20.0%
#> - Alternative response rate (H1): 40.0%
#> - Type I error rate (per cohort): 0.100
#> - Type II error rate (per cohort): 0.200
#> - Optimization criterion: Minimize expected sample size under H0
#> 
#> ### Hypothesis Testing
#> 
#> We test:
#> 
#> H0: p <= 0.20 (the treatment is not efficacious)
#> H1: p > 0.20 (the treatment is efficacious)
#> 
#> where p represents the true response rate.
#> 
#> ### Type I Error Control
#> 
#> The design is calibrated to achieve a Type I error rate of alpha = 0.100, meaning there is at most a 10.0% probability of concluding the treatment is efficacious when the true response rate is at or below the null hypothesis value.
#> 
#> ### Decision Criteria
#> 
#> This study aims to identify an improvement in response rate from 20.0% (null hypothesis) to 40.0% (alternative hypothesis) with treatment. Using a two-stage Simon optimal design with 80.0% power and 10.0% alpha (one-sided), participants will be recruited in two stages as follows:
#> 
#> **Stage 1 Enrollment and Interim Analysis:**
#> 
#> - Enroll **n1 participants** to the first stage
#> - If **(r1 + 1) or more** participants meet the primary endpoint, proceed to Stage 2
#> - If **(r1 + 1) or more** participants meet the endpoint prior to enrollment of n1 participants, accrual will continue uninterrupted from Stage 1 to Stage 2
#> - If **(r1 + 1) or more** participants have not met the endpoint at the time n1 participants have been enrolled, accrual will be held until:
#>   - **(r1 + 1) or more** participants have met the endpoint (proceed to Stage 2), OR
#>   - **n1 - (r1 + 1) or more** participants have failed to meet the endpoint (stop for futility)
#> - If an insufficient number of participants meet the endpoint to proceed to Stage 2, the trial will be stopped for futility at the conclusion of Stage 1
#> 
#> **Stage 2 Enrollment and Final Analysis:**
#> 
#> - If Stage 2 is opened, enroll additional participants to reach a maximum total of **n participants**
#> - Success is declared when **(r + 1) or more** participants meet the primary endpoint
#> - If fewer than (r + 1) participants meet the endpoint, fail to reject the null hypothesis
#> 
#> *Note: The specific design parameters (n1, r1, n, r) will be calculated using the Simon (1989) method based on the above specifications and documented in the Statistical Analysis Plan.*
#> 
#> ## Operating Characteristics
#> 
#> The design has been calibrated to achieve:
#> - Type I error rate: alpha = 0.100 (10.0% probability of false positive)
#> - Power: >= 80.0% (at alternative response rate of 40.0%)
#> - Expected sample size: Minimized under H0 (optimal design)
#> 
#> 
#> Detailed design parameters (n1, r1, n, r) are calculated using the Simon (1989) method to achieve these operating characteristics.
#> 
#> 
#> ## References
#> 
#> 1. Simon R. Optimal two-stage designs for phase II clinical trials. Control Clin Trials. 1989;10(1):1-10.
#> 2. Jung SH, Lee T, Kim K, George SL. Admissible two-stage designs for phase II cancer clinical trials. Stat Med. 2004;23(4):561-569.
#> 3. Korn EL, Freidlin B, Abrams JS, Halabi S. Design issues in randomized phase II/III trials. J Clin Oncol. 2012;30(6):667-671.
```

### Export to Multiple Formats

``` r
# Export to Word for protocol documents
export_protocol_language(design, "simon_protocol.docx")

# Export to LaTeX for publications
export_protocol_language(design, "simon_protocol.tex")

# Export to HTML for web sharing
export_protocol_language(design, "simon_protocol.html")

# Export to R Markdown for reproducible reports
export_protocol_language(design, "simon_protocol.Rmd")
```

## Parallel Basket Trial with Independent Simon Designs

### Multiple Baskets Example

For basket trials where you want independent testing per basket (no
information borrowing):

``` r
# Basket trial with 4 disease cohorts
# Each uses independent Simon design
design_basket <- simon_design(
  n_baskets = 4,
  basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
  sample_sizes = 25,
  null_response_rates = 0.20,
  alternative_response_rates = 0.40,
  alpha = 0.05 / 4,  # Bonferroni correction for FWER control
  beta = 0.20,
  design_type = "optimal"
)

print(design_basket)
#> Simon Two-Stage Design (Parallel Independent Designs)
#> =====================================================
#> 
#> Design Type: OPTIMAL 
#>   - optimal: minimizes expected sample size under H0
#>   - minimax: minimizes maximum sample size
#> 
#> Number of cohorts: 4 
#> Per-cohort error rates:
#>   - Alpha (type I): 0.0125 
#>   - Beta (type II): 0.2 
#>   - Power: 0.8 
#> 
#> Cohort specifications:
#>   NSCLC: n=25, p0=0.200
#>   SCLC: n=25, p0=0.200
#>   Melanoma: n=25, p0=0.200
#>   RCC: n=25, p0=0.200
#> 
#> Note: No information borrowing between cohorts.
#> For FWER control at level epsilon, use alpha = epsilon / n_baskets (Bonferroni).
```

### Protocol Language for Basket Trial

``` r
protocol_text_basket <- generate_protocol_language(design_basket)
cat(protocol_text_basket)
#> # Simon Two-Stage Design for Multi-Cohort Trial
#> 
#> ## Trial Overview
#> 
#> This trial uses independent Simon two-stage designs conducted in parallel across 4 cancer types or subtypes. Each cohort will be analyzed separately without borrowing information across cohorts, providing a conservative approach that protects against inappropriate pooling of heterogeneous treatment effects. The OPTIMAL design has been selected to minimize the expected sample size under the null hypothesis.
#> 
#> ## Trial Cohorts
#> 
#> 1. NSCLC (N = 25 patients)
#> 2. SCLC (N = 25 patients)
#> 3. Melanoma (N = 25 patients)
#> 4. RCC (N = 25 patients)
#> 
#> The maximum total sample size for this trial is 100 patients across 4 independent cohorts (25 patients per cohort). Each cohort uses a Simon two-stage design optimized for single-arm phase II trials.
#> 
#> ## Statistical Methodology
#> 
#> This trial uses independent Simon two-stage designs conducted in parallel for each cohort. Each cohort is analyzed separately without borrowing information across cohorts. This approach provides a conservative reference standard and is appropriate when treatment effects may be heterogeneous across cancer types.
#> 
#> ### Two-Stage Design
#> 
#> Each cohort follows a two-stage design:
#> 
#> **Stage 1**: Enroll n1 patients and observe the number of responses r1.
#> - If responses <= r1, stop for futility (cohort is not promising)
#> - If responses > r1, continue to Stage 2
#> 
#> **Stage 2**: Enroll additional patients to reach total sample size n.
#> - At final analysis, if total responses > r, reject H0 (cohort is promising)
#> - Otherwise, fail to reject H0
#> 
#> The design parameters (n1, r1, n, r) are calculated using the Simon (1989) method with the following specifications:
#> - Null response rate (H0): 20.0%
#> - Alternative response rate (H1): 40.0%
#> - Type I error rate (per cohort): 0.013
#> - Type II error rate (per cohort): 0.200
#> - Optimization criterion: Minimize expected sample size under H0
#> 
#> ### Hypothesis Testing
#> 
#> For each cohort j (j = 1, ..., 4), we test:
#> 
#> H0: p_j <= p0 (the treatment is not efficacious)
#> H1: p_j > p0 (the treatment is efficacious)
#> 
#> where p_j represents the true response rate for cohort j and p0 is the null response rate.
#> 
#> ### Multiple Testing Adjustment
#> 
#> To control the family-wise error rate (FWER) across 4 cohorts, a Bonferroni correction is applied. Each individual cohort is tested at significance level alpha = 0.0125. This ensures that the probability of falsely rejecting H0 for at least one cohort is controlled when all null hypotheses are true.
#> 
#> ### Decision Criteria
#> 
#> **Stage 1 Decision (Futility)**:
#> For each cohort, if the number of responses in the first n1 patients is <= r1, enrollment to that cohort stops and the treatment is declared not promising for that cohort. Cohorts may continue accrual uninterrupted if (r1 + 1) or more responses are observed before reaching n1 patients.
#> 
#> **Final Decision (Efficacy)**:
#> For cohorts that continue to Stage 2, if the total number of responses is (r + 1) or more across all n patients, the treatment is declared promising for that cohort (H0 is rejected).
#> 
#> ## Operating Characteristics
#> 
#> The design has been calibrated to achieve:
#> - Type I error rate per cohort: alpha = 0.0125
#> - Power per cohort: >= 80.0% (at alternative response rate)
#> - Expected sample size: Minimized under H0 (optimal design)
#> 
#> 
#> ## References
#> 
#> 1. Simon R. Optimal two-stage designs for phase II clinical trials. Control Clin Trials. 1989;10(1):1-10.
#> 2. Jung SH, Lee T, Kim K, George SL. Admissible two-stage designs for phase II cancer clinical trials. Stat Med. 2004;23(4):561-569.
#> 3. Korn EL, Freidlin B, Abrams JS, Halabi S. Design issues in randomized phase II/III trials. J Clin Oncol. 2012;30(6):667-671.
```

## Simulation Studies with Parallelization

### Operating Characteristics via Parallel Simulation

This is where `metabasket` truly shines over standalone
[`clinfun::ph2simon`](https://rdrr.io/pkg/clinfun/man/ph2simon.html):

``` r
# Set up parallel backend
library(future)
plan(multisession, workers = 4)

# Enable progress reporting
library(progressr)
handlers(global = TRUE)
handlers("progress")

# Run 10,000 simulations in parallel
# This would take much longer sequentially!
results <- simulate_basket_trial(
  design_basket,
  n_sims = 10000,
  .parallelize = TRUE,  # Key: enables parallel execution
  alpha = 0.05 / 4
)

# View operating characteristics
summary(results)

# Reset to sequential
plan(sequential)
handlers(global = FALSE)
```

### Power Curve Analysis

Generate power curves across different response rates:

``` r
# Vector of true response rates to evaluate
response_rates_seq <- seq(0.10, 0.60, by = 0.05)

# Set up parallel backend
plan(multisession, workers = 4)

# Calculate power for each scenario
power_results <- lapply(response_rates_seq, function(p_true) {
  # Create design with this true response rate
  design_scenario <- simon_design(
    n_baskets = 1,
    basket_names = "Basket",
    sample_sizes = 25,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40,
    alpha = 0.10,
    beta = 0.20,
    design_type = "optimal"
  )
  design_scenario$response_rates <- p_true
  
  # Simulate
  sim_result <- simulate_basket_trial(
    design_scenario,
    n_sims = 5000,
    .parallelize = TRUE,
    alpha = 0.10
  )
  
  # Extract power (rejection probability)
  data.frame(
    true_rate = p_true,
    power = sim_result$power_per_basket[1],
    type_i_error = if (p_true <= 0.20) sim_result$power_per_basket[1] else NA
  )
})

# Combine results
power_df <- bind_rows(power_results)

# Plot power curve
ggplot(power_df, aes(x = true_rate, y = power)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  geom_hline(yintercept = 0.10, linetype = "dashed", color = "red", 
             alpha = 0.5, size = 0.5) +
  geom_vline(xintercept = 0.20, linetype = "dashed", color = "red", 
             alpha = 0.5, size = 0.5) +
  geom_vline(xintercept = 0.40, linetype = "dashed", color = "green", 
             alpha = 0.5, size = 0.5) +
  annotate("text", x = 0.20, y = 0.95, label = "H0: p0 = 0.20", 
           hjust = 0, color = "red") +
  annotate("text", x = 0.40, y = 0.95, label = "H1: p1 = 0.40", 
           hjust = 0, color = "green") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Power Curve for Simon Optimal Design",
    subtitle = "Based on 5,000 simulations per scenario (parallelized)",
    x = "True Response Rate",
    y = "Power (Probability of Rejection)",
    caption = "Dashed lines: Type I error (α = 0.10) at null, target power at alternative"
  ) +
  theme_minimal(base_size = 12)

# Clean up
plan(sequential)
```

## Advanced: Interim Analysis with Simon Design

### Stage 1 Analysis

``` r
# Create Simon design
design <- simon_design(
  n_baskets = 1,
  basket_names = "NSCLC",
  sample_sizes = 43,
  null_response_rates = 0.20,
  alternative_response_rates = 0.40,
  alpha = 0.10,
  beta = 0.20,
  design_type = "optimal"
)

# Stage 1 data (e.g., n1 = 18 patients enrolled)
stage1_data <- basket_data(
  basket_names = "NSCLC",
  n_patients = 18,
  n_responses = 5
)

# Interim analysis
interim_result <- analyze_basket(stage1_data, design, alpha = 0.10)
print(interim_result)

# Check futility decision
if (interim_result$futility_stopped[1]) {
  cat("\nTrial stopped for futility at interim.\n")
} else {
  cat("\nTrial continues to Stage 2.\n")
  cat("Need to enroll", design$sample_sizes[1] - 18, "additional patients.\n")
}
```

### Stage 2 Analysis

``` r
# After Stage 2 enrollment complete
full_data <- basket_data(
  basket_names = "NSCLC",
  n_patients = 43,
  n_responses = 15
)

# Final analysis
final_result <- analyze_basket(
  full_data, 
  design, 
  alpha = 0.10, 
  interim_data = interim_result
)

print(final_result)
```

## Comparison with Other Basket Methods

A key use case for Simon designs in `metabasket` is as a **reference
comparator** for information-borrowing methods:

``` r
# Create designs for comparison
design_simon <- simon_design(
  n_baskets = 4,
  basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
  sample_sizes = 25,
  null_response_rates = 0.20,
  alternative_response_rates = 0.40,
  alpha = 0.05 / 4,
  beta = 0.20,
  design_type = "optimal"
)

design_bma <- basket_design(
  n_baskets = 4,
  basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
  sample_sizes = 25,
  response_rates = c(0.40, 0.40, 0.40, 0.40),
  null_response_rates = 0.20,
  design_type = "bma"
)

# Simulate both (in parallel)
plan(multisession, workers = 4)

results_simon <- simulate_basket_trial(
  design_simon, 
  n_sims = 1000, 
  .parallelize = TRUE
)

results_bma <- simulate_basket_trial(
  design_bma, 
  n_sims = 1000, 
  .parallelize = TRUE
)

# Compare operating characteristics
cat("Simon (no borrowing) - Family-wise Power:", 
    mean(results_simon$any_rejection), "\n")
cat("BMA (with borrowing) - Family-wise Power:", 
    mean(results_bma$any_rejection), "\n")

plan(sequential)
```

This shows the **gain from information borrowing** in basket trials.

## Summary

The `metabasket` package enhances
[`clinfun::ph2simon`](https://rdrr.io/pkg/clinfun/man/ph2simon.html) in
two critical ways for modern basket trial research:

### ✅ Parallel Simulation Infrastructure

- Leverage multiple CPU cores via `future` backend
- Progress tracking with `progressr`
- Consistent API with other basket designs
- Dramatically faster simulation studies

### ✅ Protocol Language Generation

- Automated translation to human-readable text
- Multi-format export (Word, LaTeX, HTML, Markdown, R Markdown)
- Bridges statistical design and clinical communication
- Version-controlled protocol documentation

### When to Use Simon Designs in metabasket

1.  **Reference comparator**: Establish performance without information
    borrowing
2.  **Independent baskets**: When borrowing is scientifically
    inappropriate
3.  **Regulatory conservatism**: When agencies prefer independent
    testing
4.  **Simulation benchmarks**: Compare information-borrowing methods
    against gold standard

### Computational Advantage

For large simulation studies (e.g., 10,000 replicates × 10 scenarios),
parallelization can reduce runtime from **hours to minutes** on
multi-core machines.

## References

Simon R (1989). Optimal two-stage designs for phase II clinical trials.
*Controlled Clinical Trials*, 10(1):1-10.

Jung SH, Lee T, Kim K, George SL (2004). Admissible two-stage designs
for phase II cancer clinical trials. *Statistics in Medicine*,
23(4):561-569.

## Session Info

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] dplyr_1.1.4           ggplot2_4.0.0         metabasket_0.0.0.9000
#> 
#> loaded via a namespace (and not attached):
#>  [1] vctrs_0.6.5        cli_3.6.5          knitr_1.50         rlang_1.1.6       
#>  [5] xfun_0.54          generics_0.1.4     S7_0.2.0           textshaping_1.0.4 
#>  [9] jsonlite_2.0.0     glue_1.8.0         htmltools_0.5.8.1  ragg_1.5.0        
#> [13] sass_0.4.10        scales_1.4.0       rmarkdown_2.30     grid_4.5.2        
#> [17] tibble_3.3.0       evaluate_1.0.5     jquerylib_0.1.4    fastmap_1.2.0     
#> [21] yaml_2.3.10        lifecycle_1.0.4    compiler_4.5.2     RColorBrewer_1.1-3
#> [25] fs_1.6.6           pkgconfig_2.0.3    farver_2.1.2       systemfonts_1.3.1 
#> [29] digest_0.6.37      R6_2.6.1           tidyselect_1.2.1   pillar_1.11.1     
#> [33] magrittr_2.0.4     bslib_0.9.0        withr_3.0.2        tools_4.5.2       
#> [37] gtable_0.3.6       pkgdown_2.2.0      cachem_1.1.0       desc_1.4.3
```
