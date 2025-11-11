# Generate pre-registration document

Creates a structured pre-registration document suitable for submission
to registries like OSF, ClinicalTrials.gov, etc.

## Usage

``` r
generate_preregistration(
  design,
  study_info = list(),
  format = c("markdown", "text")
)
```

## Arguments

- design:

  A basket_design object

- study_info:

  List with study metadata (title, PI, institution, etc.)

- format:

  Character. Output format: "markdown" or "text"

## Value

A pre-registration document string

## Examples

``` r
if (FALSE) { # \dontrun{
design <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  design_type = "cunanan"
)

study_info <- list(
  title = "Phase II Basket Trial of Drug X",
  pi = "Dr. Jane Smith",
  institution = "Academic Medical Center",
  sponsor = "Pharmaceutical Company"
)

prereg <- generate_preregistration(design, study_info)
writeLines(prereg, "preregistration.md")
} # }
```
