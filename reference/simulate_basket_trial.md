# Run complete simulation study

Runs a complete simulation study: generates data, analyzes with
specified method, and computes operating characteristics

## Usage

``` r
simulate_basket_trial(
  design,
  n_sims = 1000,
  seed = NULL,
  alpha = 0.05,
  .parallelize = FALSE,
  ...
)
```

## Arguments

- design:

  A basket_design object

- n_sims:

  Integer. Number of simulation replicates

- seed:

  Integer. Random seed for reproducibility

- alpha:

  Numeric. Significance level for hypothesis testing

- .parallelize:

  Logical. If TRUE, use parallel processing via furrr/future. Default
  FALSE for backwards compatibility. Set a parallel plan with
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  before calling if TRUE (e.g., `future::plan(future::multisession)`).
  See `future` package documentation for details.

- ...:

  Additional arguments passed to analysis functions

## Value

A simulation_results object containing operating characteristics

## Examples

``` r
design <- basket_design(
  n_baskets = 4,
  basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
  sample_sizes = 25,
  response_rates = c(0.35, 0.35, 0.35, 0.35),
  null_response_rates = 0.20,
  design_type = "bma"
)

# Sequential execution (default)
# results <- simulate_basket_trial(design, n_sims = 1000)

# Parallel execution
# future::plan(future::multisession, workers = 4)
# results <- simulate_basket_trial(design, n_sims = 1000, .parallelize = TRUE)
# future::plan(future::sequential)  # Reset to sequential

# Enable progress reporting (works with both sequential and parallel)
# progressr::handlers(global = TRUE)  # Enable for all subsequent calls
# progressr::handlers("progress")     # Use progress bar handler
# results <- simulate_basket_trial(design, n_sims = 1000, .parallelize = TRUE)
# progressr::handlers(global = FALSE) # Disable progress reporting
```
