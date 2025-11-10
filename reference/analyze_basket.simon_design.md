# Analyze basket trial using parallel Simon two-stage designs

Implements independent Simon optimal two-stage designs for each basket.
This serves as a reference design for comparison with basket trial
methods that borrow information across baskets. Each basket is analyzed
separately without any information sharing.

## Usage

``` r
# S3 method for class 'simon_design'
analyze_basket(
  data,
  design,
  ...,
  alpha = NULL,
  beta = NULL,
  design_type = NULL,
  interim_data = NULL
)
```

## Arguments

- data:

  A `basket_data` object containing the trial data

- design:

  A `simon_design` object created by
  [`simon_design()`](https://tgerke.github.io/metabasket/reference/simon_design.md)

- ...:

  Additional arguments (not currently used, for S3 consistency)

- alpha:

  Significance level for each individual basket. If not provided, uses
  alpha from design object. For family-wise error rate control at level
  epsilon across K baskets, set alpha = epsilon/K in the design.

- beta:

  Type II error rate (1 - power) for each basket. If not provided, uses
  beta from design object. Default is 0.20 (80% power).

- design_type:

  Character string indicating Simon design type. If not provided, uses
  design_type from design object.

- interim_data:

  Optional `simon_result` object with stage 1 results. If NULL, assumes
  `data` represents stage 1.

## Value

A list with class "simon_result" containing:

- `rejections`: Logical vector indicating rejected baskets

- `p_values`: P-values for each basket (from exact binomial test)

- `design_params`: Simon design parameters for each basket (r1, n1, r,
  n)

- `stage1_continue`: Logical vector indicating which baskets continued
  to stage 2

- `futility_stopped`: Logical vector indicating which baskets stopped
  for futility

## Details

The Simon two-stage design (Simon 1989) is the standard approach for
single-arm phase II trials. For basket trials, this corresponds to
running K independent Simon designs in parallel, one per basket.

**Stage 1**: Enroll n1 patients. If \<= r1 responses observed, stop for
futility. Otherwise continue to stage 2.

**Stage 2**: Enroll additional n2 = n - n1 patients. Reject null
hypothesis if total responses across both stages \> r.

The design parameters (r1, n1, r, n) are chosen to:

- Control type I error at alpha under null response rate (p0)

- Achieve power (1 - beta) under alternative response rate (p1)

- Minimize expected sample size under H0 (optimal design)

- OR minimize maximum sample size n (minimax design)

This implementation uses the clinfun::ph2simon algorithm for parameter
selection.

## References

Simon R (1989). Optimal two-stage designs for phase II clinical trials.
*Controlled Clinical Trials*, 10(1):1-10.

Jung SH, Lee T, Kim K, George SL (2004). Admissible two-stage designs
for phase II cancer clinical trials. *Statistics in Medicine*,
23(4):561-569.

## Examples

``` r
if (FALSE) { # \dontrun{
# Design with 4 cohorts using parallel Simon designs
design <- simon_design(
  n_baskets = 4,
  sample_sizes = 25,
  null_response_rates = 0.20,
  alternative_response_rates = 0.40,
  alpha = 0.05 / 4  # Bonferroni correction
)

# Stage 1 data
stage1 <- basket_data(
  basket_names = paste0("Basket", 1:4),
  n_patients = c(12, 12, 12, 12),  # Stage 1 sample sizes
  n_responses = c(5, 2, 4, 1)
)

# Interim analysis
interim <- analyze_basket(stage1, design, alpha = 0.05/4)  # Bonferroni
print(interim)

# After stage 2 data collected for continuing baskets...
# full_data <- basket_data(...)
# final <- analyze_basket(full_data, design, interim_data = interim)
} # }
```
