# Analyze basket trial using Cunanan et al. efficient design

Implements the efficient basket trial design from Cunanan et al. (2017)
that uses an interim assessment of heterogeneity via Fisher's exact test
to determine whether to pool baskets for increased power or analyze them
separately.

## Usage

``` r
# S3 method for class 'cunanan'
analyze_basket(
  data,
  design,
  ...,
  gamma = 0.52,
  alpha_s = 0.07,
  alpha_c = 0.05,
  interim_data = NULL
)
```

## Arguments

- data:

  A `basket_data` object containing the trial data

- design:

  A `basket_design` object containing the design specifications

- ...:

  Additional arguments (not currently used, for S3 consistency)

- gamma:

  Tuning parameter for heterogeneity assessment (0-1). Higher values
  favor heterogeneous design path. Default is 0.52 based on K=5 baskets.

- alpha_s:

  Significance level for separate analyses in heterogeneous path, before
  correction for multiple comparisons. Default is 0.07.

- alpha_c:

  Significance level for combined analysis in homogeneous path. Default
  is 0.05.

- interim_data:

  Optional `basket_data` object with stage 1 results. If NULL, assumes
  `data` represents stage 1.

## Value

A list with class "cunanan_result" containing:

- `rejections`: Logical vector indicating rejected baskets

- `p_values`: P-values for each basket

- `toh_pvalue`: P-value from test of homogeneity (Fisher's exact test)

- `design_path`: Either "heterogeneous" or "homogeneous"

- `continued_baskets`: Baskets that continued to stage 2

- `test_statistics`: Additional test statistics

## Details

The Cunanan design has two stages:

**Stage 1**: Enroll n1 patients per basket. Conduct Fisher's exact test
on the K x 2 contingency table of responses/non-responses across
baskets.

**Decision at interim**:

- If p-value \<= gamma: Evidence of heterogeneity → heterogeneous path

  - Continue only baskets with \>= 1 responder

  - Analyze each basket separately at end with Bonferroni correction
    (alpha_s/K\*)

- If p-value \> gamma: Evidence of homogeneity → homogeneous path

  - Continue all baskets if total responses \>= K (approximately 1 per
    basket)

  - Analyze pooled data at end with one-sample test (alpha_c)

**Stage 2**: Additional patients enrolled according to path chosen

## References

Cunanan KM, Iasonos A, Shen R, Begg CB, Gönen M (2017). An efficient
basket trial design. *Statistics in Medicine*, 36(10):1568-1579.
doi:10.1002/sim.7227

## Examples

``` r
if (FALSE) { # \dontrun{
# Design for 5 baskets
design <- basket_design(
  n_baskets = 5,
  sample_sizes = c(7, 7, 7, 7, 7),  # Stage 1
  design_type = "cunanan"
)

# Stage 1 data
stage1 <- basket_data(
  basket_names = paste0("Basket", 1:5),
  n_patients = c(7, 7, 7, 7, 7),
  n_responses = c(3, 2, 3, 1, 4)
)

# Analyze
result <- analyze_basket(stage1, design)
} # }
```
