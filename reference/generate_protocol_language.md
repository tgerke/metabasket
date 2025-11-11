# Generate protocol language for basket trial design

Generates standardized protocol text describing a basket trial design,
suitable for inclusion in study protocols and regulatory submissions

## Usage

``` r
generate_protocol_language(
  design,
  include_statistical_details = TRUE,
  include_references = TRUE
)
```

## Arguments

- design:

  A basket_design or simon_design object

- include_statistical_details:

  Logical. Whether to include technical details

- include_references:

  Logical. Whether to include literature references

## Value

A character string containing protocol language

## Examples

``` r
design <- basket_design(
  n_baskets = 4,
  basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
  sample_sizes = 25,
  response_rates = 0.35,
  null_response_rates = 0.20,
  design_type = "bma"
)

protocol_text <- generate_protocol_language(design)
cat(protocol_text)
#> # Basket Trial Design
#> 
#> ## Trial Overview
#> 
#> This is a basket trial designed to evaluate the efficacy of [DRUG NAME] across 4 cancer types or subtypes. The trial will enroll patients into 4 distinct baskets based on their cancer diagnosis:
#> 
#> 1. NSCLC (N = 25 patients)
#> 2. SCLC (N = 25 patients)
#> 3. Melanoma (N = 25 patients)
#> 4. RCC (N = 25 patients)
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
