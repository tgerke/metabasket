# Calculate conditional power for adaptive decisions

Calculates the conditional power given observed interim data, assuming a
specified effect size for stage 2. Similar to rpact's conditional power
functionality.

## Usage

``` r
calculate_conditional_power(
  interim_result,
  design,
  interim_data,
  n2_planned,
  assumed_rates
)
```

## Arguments

- interim_result:

  Interim analysis result from analyze_basket()

- design:

  The basket_design object

- interim_data:

  The basket_data from stage 1

- n2_planned:

  Vector of planned stage 2 sample sizes per basket

- assumed_rates:

  Vector of assumed response rates for stage 2

## Value

A list with conditional power for each basket

## Examples

``` r
if (FALSE) { # \dontrun{
# After interim analysis
cp <- calculate_conditional_power(
  interim_result = interim_result,
  design = design,
  interim_data = stage1_data,
  n2_planned = c(15, 15, 15, 15),
  assumed_rates = c(0.40, 0.40, 0.40, 0.40)
)
print(cp)
} # }
```
