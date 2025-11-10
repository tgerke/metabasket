# metabasket: Meta-Analysis and Comparison Framework for Basket Trial Designs

The metabasket package provides a unified interface for designing,
simulating, and analyzing basket trials using multiple established
statistical methods. Basket trials evaluate a single therapy across
multiple cancer types or subtypes simultaneously.

## Main Functions

- [`basket_design`](https://tgerke.github.io/metabasket/reference/basket_design.md):

  Create a basket trial design specification

- [`basket_data`](https://tgerke.github.io/metabasket/reference/basket_data.md):

  Create a basket trial data object

- [`simulate_responses`](https://tgerke.github.io/metabasket/reference/simulate_responses.md):

  Simulate basket trial data

- [`simulate_basket_trial`](https://tgerke.github.io/metabasket/reference/simulate_basket_trial.md):

  Run complete simulation study

- [`analyze_basket`](https://tgerke.github.io/metabasket/reference/analyze_basket.md):

  Analyze basket trial data

- [`generate_protocol_language`](https://tgerke.github.io/metabasket/reference/generate_protocol_language.md):

  Generate protocol text

## Supported Methods

- Bayesian Model Averaging (BMA) - Psioda et al. (2021)

- Multi-source Exchangeability Model (MEM) - Hobbs & Landin (2018)

- Bayesian Hierarchical Model (BHM) - Berry et al. (2013)

- Efficient Basket Design - Cunanan et al. (2017)

## Package Philosophy

The package follows test-driven development principles, with
comprehensive tests comparing results to published benchmarks. The
consistent interface across methods facilitates fair comparisons of
operating characteristics.

## See also

Useful links:

- <https://tgerke.github.io/metabasket>

- <https://github.com/tgerke/metabasket>

- Report bugs at <https://github.com/tgerke/metabasket/issues>

## Author

**Maintainer**: Travis Gerke <tgerke@mail.harvard.edu>
([ORCID](https://orcid.org/0000-0002-9500-8907))
