# Vemurafenib trial in BRAF V600 cancers

Data from a phase II basket trial of vemurafenib in 6 non-melanoma
cancer types with BRAF V600 mutations. The trial enrolled 84 patients
and observed 18 responses (overall RR = 21.4%).

## Usage

``` r
vemurafenib_trial
```

## Format

A `basket_data` object (list) with 4 components:

- basket_names:

  Character vector of 6 cancer type names

- n_patients:

  Integer vector of sample sizes per type (total = 84)

- n_responses:

  Integer vector of responses per type (total = 18)

- response_rates:

  Numeric vector of observed response rates

## Source

Hyman DM, Puzanov I, Subbiah V, et al. Vemurafenib in multiple
nonmelanoma cancers with BRAF V600 mutations. N Engl J Med.
2015;373(8):726-736. doi:10.1056/NEJMoa1502309

## Examples

``` r
data(vemurafenib_trial)
print(vemurafenib_trial)
#> Basket Trial Data
#> =================
#> 
#> Number of Baskets: 
#> 
#>   Basket  N Responses Response_Rate
#>      ATC  7         2         0.286
#>  ECD/LCH 14         6         0.429
#>      CCA  8         1         0.125
#>    CRC-V 26         1         0.038
#>   CRC-VC 10         0         0.000
#>    NSCLC 19         8         0.421
#> 
#> Total Patients: 84 
#> Total Responses: 18 
#> Overall Response Rate: 0.214 

# Summary statistics  
sum(vemurafenib_trial$n_patients)   # 84
#> [1] 84
sum(vemurafenib_trial$n_responses)  # 18
#> [1] 18
```
