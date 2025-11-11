# Generate comprehensive analysis report

Creates a detailed analysis report with tables, confidence intervals,
and adjusted statistics for basket trial results. Similar to rpact's
analysis reporting functionality.

## Usage

``` r
generate_analysis_report(
  result,
  design,
  data,
  format = c("text", "markdown", "html", "latex"),
  include_interim = TRUE,
  confidence_level = 0.95
)
```

## Arguments

- result:

  An analysis result object from analyze_basket()

- design:

  The basket_design object used

- data:

  The basket_data object analyzed

- format:

  Character. Output format: "text", "markdown", "html", or "latex"

- include_interim:

  Logical. Include interim analysis results if available

- confidence_level:

  Numeric. Confidence level for intervals (default 0.95)

## Value

A formatted report string

## Examples

``` r
if (FALSE) { # \dontrun{
design <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  design_type = "cunanan"
)

data <- basket_data(
  n_patients = c(25, 25, 25, 25),
  n_responses = c(8, 12, 6, 10)
)

result <- analyze_basket(data, design)
report <- generate_analysis_report(result, design, data)
cat(report)
} # }
```
