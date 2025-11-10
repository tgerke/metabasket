# Create a basket trial design specification

Creates a structured specification for a basket trial design that can be
used across different analysis methods (BMA, hierarchical models, etc.)

## Usage

``` r
basket_design(
  n_baskets,
  basket_names = NULL,
  sample_sizes,
  response_rates = NULL,
  null_response_rates,
  design_type = c("bma", "mem", "bhm", "cunanan"),
  design_params = list()
)
```

## Arguments

- n_baskets:

  Integer. Number of baskets (cancer types/subtypes) in the trial

- basket_names:

  Character vector. Names of the baskets

- sample_sizes:

  Integer vector. Sample size per basket (can be single value or vector)

- response_rates:

  Numeric vector. True response rates under alternative (for simulation)

- null_response_rates:

  Numeric vector. Null response rates to test against

- design_type:

  Character. Type of design: "bma" (Bayesian model averaging), "mem"
  (multi-source exchangeability), "bhm" (Bayesian hierarchical model),
  or "cunanan" (Cunanan et al. 2017)

- design_params:

  List. Additional design-specific parameters

## Value

An object of class 'basket_design' containing the trial specification

## Examples

``` r
# Create a simple 4-basket design
design <- basket_design(
  n_baskets = 4,
  basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
  sample_sizes = 25,
  response_rates = c(0.35, 0.35, 0.35, 0.35),
  null_response_rates = 0.20,
  design_type = "bma"
)
```
