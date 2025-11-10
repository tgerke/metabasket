# Analyze basket trial using Bayesian Hierarchical Model

Wrapper for bhmbasket package implementing Bayesian hierarchical
modeling from Berry et al. (2013) and Neuenschwander et al. (2016).

This method uses MCMC sampling via JAGS to estimate posterior
distributions with hierarchical borrowing of information across baskets.

## Usage

``` r
# S3 method for class 'bhm'
analyze_basket(data, design, ...)
```

## Arguments

- data:

  A basket_data object containing trial results

- design:

  A basket_design object with design_type = "bhm"

- ...:

  Additional arguments (currently unused)

## Value

A bhm_result object containing:

- posterior_means:

  Posterior mean response rates for each basket

- posterior_probs:

  Posterior P(rate \> null_rate) for each basket

- rejections:

  Logical vector of rejection decisions

- basket_names:

  Names of baskets

- method:

  Analysis method ("berry", "exnex", etc.)

- threshold:

  Decision threshold used

## Details

**System Requirements:** This method requires JAGS (Just Another Gibbs
Sampler) to be installed on your system. Install from
<https://mcmc-jags.sourceforge.io/>.

The bhmbasket package provides several BHM methods:

- "berry": Berry et al. (2013) exchangeability model

- "exnex": Neuenschwander et al. (2016) EX-NEX model

- "exnex_adj": Combination approach

- "pooled": Full pooling across baskets

- "stratified": No pooling (independent analyses)

## References

Berry SM, Broglio KR, Groshen S, Berry DA (2013). "Bayesian hierarchical
modeling of patient subpopulations: Efficient designs of Phase II
oncology clinical trials." Clinical Trials, 10(5), 720-734.

Neuenschwander B, Wandel S, Roychoudhury S, Bailey S (2016). "Robust
exchangeability designs for early phase clinical trials with multiple
strata." Pharmaceutical Statistics, 15(2), 123-134.

## Examples

``` r
if (FALSE) { # \dontrun{
# Note: Requires JAGS installation
design_bhm <- basket_design(
  design_type = "bhm",
  n_baskets = 3,
  n_patients_per_basket = 20,
  null_response_rates = rep(0.15, 3),
  design_params = list(
    method = "berry",
    n_mcmc_iterations = 5000,
    evidence_level = 0.1
  )
)

data_bhm <- basket_data(
  n_patients = c(20, 20, 20),
  n_responses = c(8, 10, 12),
  basket_names = c("Basket A", "Basket B", "Basket C")
)

result_bhm <- analyze_basket(data_bhm, design_bhm)
print(result_bhm)
extract_rejections(result_bhm)
} # }
```
