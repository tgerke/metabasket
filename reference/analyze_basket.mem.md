# Multi-source Exchangeability Model (MEM) Analysis Wrapper

Analyzes basket trial data using the Multi-source Exchangeability Model
(MEM) from the basket package (Hobbs & Landin 2018, Kaizer et al. 2018).

## Usage

``` r
# S3 method for class 'mem'
analyze_basket(data, design, ...)
```

## Arguments

- data:

  A basket_data object containing trial data

- design:

  A basket_design object with design_type = "mem"

- ...:

  Additional arguments (for S3 consistency)

## Value

A mem_result object containing:

- posterior_probs:

  Posterior probabilities that each basket exceeds null rate

- posterior_means:

  Posterior mean response rates for each basket

- posterior_medians:

  Posterior median response rates for each basket

- hpd_intervals:

  Highest posterior density intervals

- rejections:

  Logical vector indicating which baskets reject null

- basket_names:

  Names of baskets

- exchangeability_matrix:

  Matrix of posterior exchangeability probabilities (PEP)

- alpha:

  Significance threshold used

- raw_result:

  Raw output from basket::mem_exact()

## Details

The MEM approach learns the exchangeability structure directly from the
data, allowing for flexible information borrowing between similar
baskets while protecting against borrowing from dissimilar baskets.

The method requires the basket package to be installed. Prior
parameters:

- shape1: First shape parameter for beta prior (default 0.5)

- shape2: Second shape parameter for beta prior (default 0.5)

- prior: Prior inclusion probability matrix for basket pairs

- hpd_alpha: HPD credible interval level (default 0.05)

## References

Hobbs BP, Landin R (2018). Bayesian basket trial design with
exchangeability monitoring. Statistics in Medicine, 37(25):3557-3572.

Kaizer AM, Koopmeiners JS, Hobbs BP (2018). Bayesian hierarchical
modeling based on multisource exchangeability. Biostatistics,
19(2):169-184.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires basket package
library(basket)

design <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  null_response_rates = 0.20,
  design_type = "mem"
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
