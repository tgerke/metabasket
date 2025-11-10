# Create Simon two-stage design specification

Creates a design object for parallel independent Simon two-stage
designs. This is not a basket design (no information borrowing), but is
included as a reference comparator to evaluate information borrowing
methods.

## Usage

``` r
simon_design(
  n_baskets,
  basket_names = NULL,
  sample_sizes,
  null_response_rates,
  alternative_response_rates,
  alpha = 0.05,
  beta = 0.2,
  design_type = c("optimal", "minimax")
)
```

## Arguments

- n_baskets:

  Integer, number of independent cohorts/baskets

- basket_names:

  Optional character vector of cohort names

- sample_sizes:

  Integer vector of total sample sizes per cohort. If length 1, same
  size used for all cohorts.

- null_response_rates:

  Numeric vector of null response rates (p0). If length 1, same p0 used
  for all cohorts.

- alternative_response_rates:

  Numeric vector of alternative response rates (p1). If length 1, same
  p1 used for all cohorts. Must be \> null_response_rates.

- alpha:

  Significance level per cohort (default 0.05)

- beta:

  Type II error rate per cohort (default 0.20)

- design_type:

  Character, "optimal" (default, minimizes EN) or "minimax" (minimizes
  maximum n)

## Value

An object of class `simon_design` containing:

- `n_baskets`: Number of independent cohorts

- `basket_names`: Names of cohorts

- `sample_sizes`: Total sample sizes

- `null_response_rates`: Null response rates

- `alpha`: Significance level

- `beta`: Type II error rate

- `design_type`: Design optimization criterion

## Details

Simon two-stage designs (Simon 1989) are the standard for single-arm
phase II trials. For basket trials, running parallel Simon designs
provides a reference that controls per-cohort error rates without
borrowing information.

Each cohort operates independently:

- Stage 1: Enroll n1 patients. Stop for futility if \<= r1 responses.

- Stage 2: Enroll n2 = n - n1 additional patients.

- Decision: Reject H0 if total responses \> r.

Use Bonferroni correction for family-wise error control: Set alpha =
FWER / n_baskets.

## References

Simon R (1989). Optimal two-stage designs for phase II clinical trials.
*Controlled Clinical Trials*, 10(1):1-10.

## Examples

``` r
# Design for 4 independent cohorts
design <- simon_design(
  n_baskets = 4,
  basket_names = paste0("Cohort", 1:4),
  sample_sizes = 25,
  null_response_rates = 0.20,
  alternative_response_rates = 0.40,
  alpha = 0.05 / 4,  # Bonferroni for FWER control
  beta = 0.20
)

print(design)
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
#>   Cohort1: n=25, p0=0.200
#>   Cohort2: n=25, p0=0.200
#>   Cohort3: n=25, p0=0.200
#>   Cohort4: n=25, p0=0.200
#> 
#> Note: No information borrowing between cohorts.
#> For FWER control at level epsilon, use alpha = epsilon / n_baskets (Bonferroni).
```
