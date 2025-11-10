# Introduction to metabasket

``` r
library(metabasket)
```

## What are Basket Trials?

Basket trials are a type of master protocol that evaluates a single
therapy across multiple cancer types or subtypes simultaneously. The
term “basket” refers to grouping patients with different cancers that
share a common molecular alteration or biomarker.

### Key Features

- **Single Treatment**: All patients receive the same investigational
  therapy
- **Multiple Baskets**: Each basket represents a different cancer type
  or subtype
- **Information Borrowing**: Statistical methods can borrow information
  across baskets to improve efficiency
- **Efficiency**: One protocol, one study team, faster enrollment

### Example Trials

1.  **Imatinib in Sarcomas** (Chugh et al. 2009): Evaluated imatinib
    across 10 sarcoma subtypes
2.  **Vemurafenib in BRAF V600 Cancers** (Hyman et al. 2015): Tested
    vemurafenib in 6 non-melanoma cancers
3.  **Larotrectinib in TRK Fusion Cancers** (Drilon et al. 2018):
    Studied TRK inhibitor across 12 cancer types

## The metabasket Package

The `metabasket` package provides a unified framework for:

1.  **Designing** basket trials with different statistical methods
2.  **Simulating** trial data to evaluate operating characteristics
3.  **Analyzing** basket trial data with consistent interfaces
4.  **Comparing** different design approaches
5.  **Generating** protocol language for regulatory submissions

## System Requirements

Most methods in `metabasket` work out of the box. However, the
**Bayesian Hierarchical Model (BHM)** methods require **JAGS** (Just
Another Gibbs Sampler) to be installed on your system.

### Installing JAGS

**macOS (using Homebrew):**

``` bash
brew install jags
```

After installing JAGS, the `rjags` R package may need symlinks if
Homebrew installed JAGS in `/opt/homebrew`:

``` bash
sudo mkdir -p /usr/local/lib
sudo ln -sf /opt/homebrew/Cellar/jags/*/lib/libjags.4.dylib /usr/local/lib/libjags.4.dylib
sudo ln -sf /opt/homebrew/Cellar/jags/*/lib/JAGS /usr/local/lib/JAGS
```

Then install the R package:

``` r
install.packages("rjags")
```

**Windows:**

1.  Download the JAGS installer from:
    <https://mcmc-jags.sourceforge.io/>
2.  Run the installer (choose appropriate 32-bit or 64-bit version)
3.  Install the R package:

``` r
install.packages("rjags")
```

**Linux (Ubuntu/Debian):**

``` bash
sudo apt-get install jags
```

Then in R:

``` r
install.packages("rjags")
```

**Verifying Installation:**

``` r
library(rjags)
# Should load without errors
```

If BHM methods are not needed, all other methods (BMA, MEM, Simon,
Cunanan) work without JAGS.

## Basic Workflow

### 1. Define a Basket Trial Design

``` r
# Create a simple 4-basket design
design <- basket_design(
  n_baskets = 4,
  basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
  sample_sizes = 25,
  response_rates = c(0.35, 0.35, 0.35, 0.35),
  null_response_rates = 0.20,
  design_type = "bma"
)

print(design)
#> Basket Trial Design
#> ==================
#> 
#> Design Type: bma 
#> Number of Baskets: 4 
#> 
#> Basket Details:
#>    Basket  N Null_Rate Response_Rate
#>     NSCLC 25       0.2          0.35
#>      SCLC 25       0.2          0.35
#>  Melanoma 25       0.2          0.35
#>       RCC 25       0.2          0.35
```

### 2. Simulate Trial Data

``` r
# Generate simulated trials
set.seed(123)
simulated_data <- simulate_responses(design, n_sims = 10)

# Look at first simulation
print(simulated_data[[1]])
#> Basket Trial Data
#> =================
#> 
#> Number of Baskets: 4 
#> 
#>    Basket  N Responses Response_Rate
#>     NSCLC 25         7          0.28
#>      SCLC 25        11          0.44
#>  Melanoma 25         8          0.32
#>       RCC 25        12          0.48
#> 
#> Total Patients: 100 
#> Total Responses: 38 
#> Overall Response Rate: 0.38
```

### 3. Analyze Data

``` r
# Note: These functions will be fully implemented with package integrations

# Analyze a single trial
results <- analyze_basket(simulated_data[[1]], design)

# Run full simulation study
sim_results <- simulate_basket_trial(design, n_sims = 1000)

# View operating characteristics
print(sim_results)

# For large simulations, enable parallel processing for speedup
# future::plan(future::multisession, workers = 4)
# sim_results_parallel <- simulate_basket_trial(design, n_sims = 5000, .parallelize = TRUE)
# future::plan(future::sequential)  # Reset when done
```

## Working with Real Data

You can create basket data objects from real trial results:

``` r
# Imatinib trial data (Chugh et al. 2009)
imatinib_data <- basket_data(
  basket_names = c("Angiosarcoma", "Ewing", "Fibrosarcoma", "Leiomyosarcoma",
                   "Liposarcoma", "MFH", "Osteosarcoma", "MPNST",
                   "Rhabdomyosarcoma", "Synovial"),
  n_patients = c(15, 13, 12, 28, 29, 29, 26, 5, 2, 20),
  n_responses = c(2, 0, 1, 6, 7, 3, 5, 1, 0, 3)
)

print(imatinib_data)
#> Basket Trial Data
#> =================
#> 
#> Number of Baskets: 10 
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
```

## Comparing Design Methods

The package supports multiple statistical approaches:

### Bayesian Model Averaging (BMA)

``` r
design_bma <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  response_rates = c(0.35, 0.35, 0.20, 0.20),
  null_response_rates = 0.20,
  design_type = "bma"
)
```

BMA considers all possible partitions of baskets and averages results
across partitions weighted by their posterior probabilities.

### Multi-source Exchangeability Model (MEM)

``` r
design_mem <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  response_rates = c(0.35, 0.35, 0.20, 0.20),
  null_response_rates = 0.20,
  design_type = "mem"
)
```

MEM dynamically learns which baskets are exchangeable based on observed
data.

### Bayesian Hierarchical Model (BHM)

``` r
design_bhm <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  response_rates = c(0.35, 0.35, 0.20, 0.20),
  null_response_rates = 0.20,
  design_type = "bhm"
)
```

BHM assumes basket response rates come from a common distribution,
enabling shrinkage estimation.

## Generating Protocol Language

The package can generate standardized protocol text:

``` r
protocol_text <- generate_protocol_language(
  design_bma,
  include_statistical_details = TRUE,
  include_references = TRUE
)

# Display protocol text
cat(protocol_text)
#> # Basket Trial Design
#> 
#> ## Trial Overview
#> 
#> This is a basket trial designed to evaluate the efficacy of [DRUG NAME] across 4 cancer types or subtypes. The trial will enroll patients into 4 distinct baskets based on their cancer diagnosis:
#> 
#> 1. Basket_1 (N = 25 patients)
#> 2. Basket_2 (N = 25 patients)
#> 3. Basket_3 (N = 25 patients)
#> 4. Basket_4 (N = 25 patients)
#> 
#> The total sample size for this trial is 100 patients (25 patients per basket). This sample size was selected to provide adequate power to detect clinically meaningful treatment effects while accounting for information borrowing across baskets.
#> 
#> ## Statistical Methodology
#> 
#> The trial will use a Bayesian Model Averaging for the analysis of treatment efficacy. The Bayesian model averaging (BMA) approach considers multiple possible partitions of the baskets, where baskets within the same partition are assumed to have identical response rates. The posterior probability of treatment efficacy in each basket is computed as a weighted average across all partitions, where the weights correspond to the posterior probabilities of the partitions given the observed data. This approach provides adaptive borrowing of information across baskets based on the similarity of their observed response rates.
#> 
#> #### Technical Details
#> 
#> Prior distributions will be placed on the response rates within each partition, and a prior will be specified over the space of all possible partitions. The posterior distribution will be computed using analytical or numerical methods, and decisions will be based on the posterior probability that the response rate exceeds the null rate in each basket.
#> 
#> ### Hypothesis Testing
#> 
#> For each basket j (j = 1, ..., 4), we will test the following hypotheses:
#> 
#> H0: pi_j <= 20.0% (the treatment is not efficacious)
#> H1: pi_j > 20.0% (the treatment is efficacious)
#> 
#> where pi_j represents the true response rate for basket j.
#> 
#> ### Decision Criteria
#> 
#> At the completion of enrollment, the posterior probability that H1 is true will be computed for each basket using the observed data and the specified statistical model. A basket will be declared promising (i.e., H0 will be rejected) if the posterior probability exceeds a prespecified threshold gamma.
#> 
#> The threshold gamma will be calibrated through simulation studies to achieve desired operating characteristics (e.g., family-wise error rate <= 5%, adequate power). The final threshold will be specified in the Statistical Analysis Plan prior to unblinding.
#> 
#> 
#> 
#> ## Operating Characteristics
#> 
#> The design has been calibrated through simulation studies to maintain appropriate Type I error control while maximizing power to detect treatment effects. Detailed operating characteristics are available in the Statistical Analysis Plan.
#> 
#> 
#> ## References
#> 
#> 1. Zhou T, Ji Y. Bayesian Methods for Information Borrowing in Basket Trials: An Overview. Cancers. 2024;16(2):251.
#> 2. Hobbs BP, Pestana RC, Zabor EC, Kaizer AM, Hong DS. Basket trials: Review of current practice and innovations for future trials. J Clin Oncol. 2022;40(28):3520-3528.
#> 3. Psioda MA, Xu J, Jiang Q, Ke C, Yang Z, Ibrahim JG. Bayesian adaptive basket trial design using model averaging. Biostatistics. 2021;22(1):19-34.
```

``` r
# Export to file
export_protocol_language(design_bma, file = "basket_protocol.md")
```

## Understanding Operating Characteristics

Key metrics for basket trials:

### Basket-Specific Metrics

- **Power**: Probability of correctly identifying a promising basket
- **Type I Error**: Probability of falsely declaring a non-promising
  basket as promising

### Family-Wise Metrics

- **FWER**: Family-wise error rate (probability of ≥1 false positive)
- **FWP-D**: Disjunctive power (probability of detecting ≥1 promising
  basket)
- **FWP-C**: Conjunctive power (probability of detecting all promising
  baskets)

## The Cunanan Efficient Design

The Cunanan et al. (2017) design offers a frequentist alternative with
an adaptive two-stage approach:

### Key Features

1.  **Interim Heterogeneity Test**: Uses Fisher’s exact test to assess
    homogeneity
2.  **Two Design Paths**:
    - **Heterogeneous Path**: Analyze baskets separately with Bonferroni
      correction
    - **Homogeneous Path**: Pool all baskets for increased power
3.  **Sample Size Efficiency**: Can achieve high power with fewer
    patients when treatment effect is homogeneous

### Example Usage

``` r
# Design for 5 baskets (Cunanan default parameters)
design_cunanan <- basket_design(
  n_baskets = 5,
  sample_sizes = rep(7, 5),  # Stage 1: 7 patients per basket
  null_response_rates = 0.15,
  response_rates = 0.45,
  design_type = "cunanan",
  design_params = list(
    r_s = 1,      # Min responses to continue individual basket
    r_c = 5,      # Min total responses to continue all baskets
    n2 = 15,      # Stage 2 sample size per continuing basket
    gamma = 0.52, # Heterogeneity test threshold
    alpha_s = 0.07, # Separate analysis significance level
    alpha_c = 0.05  # Combined analysis significance level
  )
)

# Stage 1 data (interim analysis)
stage1_data <- basket_data(
  basket_names = paste0("Basket", 1:5),
  n_patients = rep(7, 5),
  n_responses = c(4, 4, 1, 0, 1)
)

# Perform interim analysis
interim_result <- analyze_basket(stage1_data, design_cunanan)
print(interim_result)

# Continues to stage 2 based on interim result...
# After collecting additional data:
# final_data <- basket_data(...)
# final_result <- analyze_basket(final_data, design_cunanan, 
#                                interim_data = interim_result)
```

### When to Use Cunanan Design

**Advantages:** - Frequentist framework (may be preferred by some
reviewers) - Explicit control of FWER at target level - Efficient when
treatment effect is homogeneous - Clear decision rules

**Considerations:** - Less power when effect is in only 1 basket -
Requires two-stage implementation - Less flexible than Bayesian
approaches for complex borrowing

## Simon Two-Stage Design (Reference)

The Simon design serves as a reference comparator, analyzing each basket
independently:

### Key Features

1.  **No Information Borrowing**: Each basket analyzed separately
2.  **Optimal or Minimax**: Choose design that minimizes expected N or
    maximum N
3.  **Futility Stopping**: Two-stage design with interim futility check
    per basket
4.  **FWER Control**: Bonferroni correction across baskets

### Example Usage

``` r
# Parallel Simon designs for 4 baskets
design_simon <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  null_response_rates = 0.20,
  response_rates = 0.40,
  design_type = "simon"
)

# Stage 1
stage1 <- basket_data(
  basket_names = paste0("Basket", 1:4),
  n_patients = c(12, 12, 12, 12),
  n_responses = c(6, 2, 5, 1)
)

# Interim analysis with Bonferroni correction
interim <- analyze_basket(stage1, design_simon, 
                         alpha = 0.05/4,  # FWER control
                         beta = 0.20)
print(interim)

# Compare with basket designs
# Basket designs typically require fewer patients due to information borrowing
```

### When to Use Simon Design

**Use as reference when:** - Comparing efficiency of basket designs -
Conservative approach needed (no borrowing assumptions) - Strong
heterogeneity expected (borrowing inappropriate)

**Limitations:** - Requires more patients than basket designs when
treatment is homogeneous - No adaptation based on similarity across
baskets - Conservative (may miss opportunities for information sharing)

## Design Considerations

### When to Borrow Information

Information borrowing is beneficial when:

- Treatment response rates are expected to be similar across baskets
- Some baskets have small sample sizes
- The mechanism of action suggests similar efficacy

Be cautious when:

- Substantial heterogeneity is expected
- Risk of inflated Type I error is unacceptable
- Strong prior evidence suggests differential effects

### Choosing Sample Sizes

Factors to consider:

1.  Expected response rates under H0 and H1
2.  Desired power and Type I error control
3.  Degree of information borrowing
4.  Prevalence of each cancer type
5.  Enrollment feasibility

### Setting Null Response Rates

The null rate (p0) typically represents:

- Historical response rate to standard of care
- Minimum clinically interesting response rate
- May vary by basket if standard of care differs

## Next Steps

For more details, see these vignettes:

- `vignette("simulation-studies")`: Conducting simulation studies
- `vignette("method-comparison")`: Comparing statistical methods
- `vignette("real-data-analysis")`: Analyzing published basket trials

## References

1.  **Zhou T, Ji Y.** Bayesian Methods for Information Borrowing in
    Basket Trials: An Overview. *Cancers*. 2024;16(2):251.

2.  **Hobbs BP, et al.** Basket trials: Review of current practice and
    innovations for future trials. *J Clin Oncol*.
    2022;40(28):3520-3528.

3.  **Psioda MA, et al.** Bayesian adaptive basket trial design using
    model averaging. *Biostatistics*. 2021;22(1):19-34.

4.  **Hobbs BP, Landin R.** Bayesian basket trial design with
    exchangeability monitoring. *Stat Med*. 2018;37(25):3557-3572.

5.  **Berry SM, et al.** Bayesian hierarchical modeling of patient
    subpopulations: Efficient designs of phase II oncology clinical
    trials. *Clin Trials*. 2013;10(5):720-734.
