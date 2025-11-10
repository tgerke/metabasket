# Simulate basket trial data

Generates simulated response data for a basket trial based on specified
response rates

## Usage

``` r
simulate_responses(design, n_sims = 1000, seed = NULL)
```

## Arguments

- design:

  A basket_design object or parameters for trial design

- n_sims:

  Integer. Number of simulation replicates

- seed:

  Integer. Random seed for reproducibility

## Value

A list of basket_data objects, one for each simulation

## Examples

``` r
design <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  response_rates = c(0.35, 0.35, 0.20, 0.20),
  null_response_rates = 0.20,
  design_type = "bma"
)

sims <- simulate_responses(design, n_sims = 100)
```
