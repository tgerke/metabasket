# Create standard simulation scenarios

Creates pre-defined simulation scenarios commonly used in basket trial
literature

## Usage

``` r
create_scenario(
  scenario = c("global_null", "global_alternative", "mixed_half", "mixed_heterogeneous"),
  n_baskets = 4,
  null_rate = 0.2,
  alt_rate = 0.35
)
```

## Arguments

- scenario:

  Character. One of "global_null", "global_alternative", "mixed_half",
  "mixed_heterogeneous"

- n_baskets:

  Integer. Number of baskets

- null_rate:

  Numeric. Null response rate

- alt_rate:

  Numeric. Alternative response rate

## Value

A numeric vector of response rates

## Examples

``` r
# Global null scenario (all baskets ineffective)
rates_null <- create_scenario("global_null", n_baskets = 4, null_rate = 0.20)

# Global alternative (all baskets effective)
rates_alt <- create_scenario("global_alternative", n_baskets = 4, 
                             null_rate = 0.20, alt_rate = 0.35)
```
