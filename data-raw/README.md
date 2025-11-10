# Data Sources

This directory contains scripts to generate example datasets included in the package.

## Example Datasets

### Imatinib Trial (`imatinib_trial.rda`)

**Source:** Chugh R, Wathen JK, Maki RG, et al. Phase II multicenter trial of imatinib in 10 histologic subtypes of sarcoma using a Bayesian hierarchical statistical model. *J Clin Oncol*. 2009;27(19):3148-3153. doi:10.1200/JCO.2008.20.9054

**Description:** Phase II basket trial of imatinib mesylate in 10 sarcoma subtypes. Total N=179 patients, 28 responses (15.6% overall response rate).

**Usage:** Demonstrates basket trial analysis with moderate heterogeneity across cancer subtypes.

### Vemurafenib Trial (`vemurafenib_trial.rda`)

**Source:** Hyman DM, Puzanov I, Subbiah V, et al. Vemurafenib in multiple nonmelanoma cancers with BRAF V600 mutations. *N Engl J Med*. 2015;373(8):726-736. doi:10.1056/NEJMoa1502309

**Description:** Phase II basket trial of vemurafenib in 6 non-melanoma cancer types with BRAF V600 mutations. Total N=84 patients, 18 responses (21.4% overall response rate).

**Usage:** Demonstrates basket trial with varying response rates across molecularly-defined cohorts.

## Generating the Data

To regenerate the datasets:

```r
source("data-raw/create_example_data.R")
```

This will create/overwrite:
- `data/imatinib_trial.rda`
- `data/vemurafenib_trial.rda`

## Notes

These datasets are stored as `basket_data` objects for direct use with package analysis functions. They represent real clinical trial results and are useful for:

- Testing package functions
- Demonstrating analysis methods
- Comparing different basket trial approaches
- Teaching basket trial concepts

The data are publicly available summary statistics from published trials, not individual patient data.
