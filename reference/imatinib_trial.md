# Imatinib trial in sarcomas

Data from a phase II multicenter basket trial of imatinib mesylate in 10
histologic subtypes of sarcoma using a Bayesian hierarchical model. The
trial enrolled 179 patients and observed 28 responses (overall RR =
15.6%).

## Usage

``` r
imatinib_trial
```

## Format

A `basket_data` object (list) with 4 components:

- basket_names:

  Character vector of 10 sarcoma subtype names

- n_patients:

  Integer vector of sample sizes per subtype (total = 179)

- n_responses:

  Integer vector of responses per subtype (total = 28)

- response_rates:

  Numeric vector of observed response rates

## Source

Chugh R, Wathen JK, Maki RG, et al. Phase II multicenter trial of
imatinib in 10 histologic subtypes of sarcoma using a Bayesian
hierarchical statistical model. J Clin Oncol. 2009;27(19):3148-3153.
doi:10.1200/JCO.2008.20.9054

## Examples

``` r
data(imatinib_trial)
print(imatinib_trial)
#> Basket Trial Data
#> =================
#> 
#> Number of Baskets: 
#> 
#>            Basket  N Responses Response_Rate
#>      Angiosarcoma 15         2         0.133
#>             Ewing 13         0         0.000
#>      Fibrosarcoma 12         1         0.083
#>    Leiomyosarcoma 28         6         0.214
#>       Liposarcoma 29         7         0.241
#>               MFH 29         3         0.103
#>      Osteosarcoma 26         5         0.192
#>             MPNST  5         1         0.200
#>  Rhabdomyosarcoma  2         0         0.000
#>          Synovial 20         3         0.150
#> 
#> Total Patients: 179 
#> Total Responses: 28 
#> Overall Response Rate: 0.156 

# Summary statistics
sum(imatinib_trial$n_patients)    # 179
#> [1] 179
sum(imatinib_trial$n_responses)   # 28
#> [1] 28
```
