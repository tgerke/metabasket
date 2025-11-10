# Calculate Simon two-stage design parameters

Uses the algorithm from Simon (1989) to find optimal or minimax
two-stage designs. This is a simplified implementation; for production
use, consider using clinfun::ph2simon.

## Usage

``` r
calculate_simon_design(
  p0,
  p1,
  alpha = 0.05,
  beta = 0.2,
  design_type = c("optimal", "minimax")
)
```

## Arguments

- p0:

  Null response rate

- p1:

  Alternative response rate (should be \> p0)

- alpha:

  Significance level (type I error)

- beta:

  Type II error rate (1 - power)

- design_type:

  "optimal" or "minimax"

## Value

List with Simon design parameters: r1, n1, r, n, EN (expected sample
size)
