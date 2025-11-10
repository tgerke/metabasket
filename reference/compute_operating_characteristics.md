# Compute operating characteristics from simulation results

Calculates type I error, power, FWER, etc. from simulation results

## Usage

``` r
compute_operating_characteristics(results, design, alpha = 0.05)
```

## Arguments

- results:

  List of analysis results from simulations

- design:

  Original basket_design object

- alpha:

  Significance level

## Value

A list containing operating characteristics
