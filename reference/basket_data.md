# Create a basket trial data object

Creates a structured data object representing observed data from a
basket trial

## Usage

``` r
basket_data(basket_names, n_patients, n_responses)
```

## Arguments

- basket_names:

  Character vector. Names of the baskets

- n_patients:

  Integer vector. Number of patients per basket

- n_responses:

  Integer vector. Number of responses per basket

## Value

An object of class 'basket_data' containing the trial data

## Examples

``` r
# Imatinib trial data from Chugh et al. 2009
data <- basket_data(
  basket_names = c("Angiosarcoma", "Ewing", "Fibrosarcoma"),
  n_patients = c(15, 13, 12),
  n_responses = c(2, 0, 1)
)
```
