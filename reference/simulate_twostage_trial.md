# Simulate two-stage basket trial (e.g., Cunanan design)

Handles simulation for designs requiring two stages: Stage 1: Initial
enrollment and interim analysis Stage 2: Conditional enrollment for
continued baskets

## Usage

``` r
simulate_twostage_trial(design, n_sims, seed, alpha, .parallelize = FALSE, ...)
```

## Arguments

- design:

  A basket_design object (must be two-stage design)

- n_sims:

  Number of simulations

- seed:

  Random seed for reproducibility

- alpha:

  Significance level for hypothesis tests

- .parallelize:

  Logical. If TRUE, use parallel processing via furrr/future

- ...:

  Additional arguments passed to analyze_basket

## Value

A simulation_results object
