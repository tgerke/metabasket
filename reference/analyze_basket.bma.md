# Bayesian Model Averaging Analysis Wrapper

Analyzes basket trial data using Bayesian Model Averaging (BMA) from the
bmabasket package (Psioda et al. 2021).

## Usage

``` r
# S3 method for class 'bma'
analyze_basket(data, design, ...)
```

## Arguments

- data:

  A basket_data object containing trial data

- design:

  A basket_design object with design_type = "bma"

- ...:

  Additional arguments (for S3 consistency)

## Value

A bma_result object containing:

- posterior_probs:

  Posterior probabilities that each basket exceeds null rate

- posterior_means:

  Posterior mean response rates for each basket

- rejections:

  Logical vector indicating which baskets reject null

- basket_names:

  Names of baskets

- alpha:

  Significance threshold used

- raw_result:

  Raw output from bmabasket::bma()

## Details

The BMA method (Psioda et al. 2021) averages over all possible
partitions of baskets, where baskets within the same partition share the
same response rate. This provides adaptive information borrowing that is
data-driven.

The method requires the bmabasket package to be installed. Prior
parameters:

- mu0: Prior mean (default 0.5)

- phi0: Prior dispersion (default 1)

- pmp0: Model probability parameter (default 1, use 0 for uniform prior)

## References

Psioda MA, Xu J, Jiang Q, Ke C, Yang Z, Ibrahim JG (2021). Bayesian
adaptive basket trial design using model averaging. Biostatistics,
22(1):19-34. doi:10.1093/biostatistics/kxz014

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires bmabasket package
library(bmabasket)

design <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  null_response_rates = 0.20,
  design_type = "bma"
)

data <- basket_data(
  basket_names = paste0("Basket", 1:4),
  n_patients = c(25, 25, 25, 25),
  n_responses = c(8, 10, 6, 12)
)

result <- analyze_basket(data, design)
print(result)
} # }
```
