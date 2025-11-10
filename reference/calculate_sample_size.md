# Calculate sample size for basket trial

Provides sample size recommendations based on desired operating
characteristics

## Usage

``` r
calculate_sample_size(
  n_baskets,
  null_rate,
  alt_rate,
  power = 0.8,
  alpha = 0.05,
  method = "bma"
)
```

## Arguments

- n_baskets:

  Integer. Number of baskets

- null_rate:

  Numeric. Null response rate

- alt_rate:

  Numeric. Alternative response rate

- power:

  Numeric. Desired power (0-1)

- alpha:

  Numeric. Significance level

- method:

  Character. Design method to use

## Value

Recommended sample size per basket

## Examples

``` r
# Calculate sample size for BMA design
n <- calculate_sample_size(
  n_baskets = 4,
  null_rate = 0.20,
  alt_rate = 0.35,
  power = 0.80,
  alpha = 0.05,
  method = "bma"
)
#> Recommended sample size: 91 patients per basket
#> Total trial size: 364 patients
#> Note: This is a preliminary estimate. Conduct simulations to verify operating characteristics.
```
