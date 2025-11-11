# Export protocol language to file

Export protocol language to file

## Usage

``` r
export_protocol_language(design, file, ...)
```

## Arguments

- design:

  A basket_design object

- file:

  Character. Output file path. Extension determines format: .md =
  markdown, .html = HTML, .tex = LaTeX, .docx = Word, .txt = plain text,
  .Rmd = R Markdown with embedded R code

- ...:

  Additional arguments passed to generate_protocol_language()

## Value

Invisibly returns the protocol text

## Examples

``` r
if (FALSE) { # \dontrun{
design <- basket_design(
  n_baskets = 4,
  sample_sizes = 25,
  design_type = "cunanan"
)

# Export as markdown
export_protocol_language(design, "protocol.md")

# Export as HTML
export_protocol_language(design, "protocol.html")

# Export as Word document
export_protocol_language(design, "protocol.docx")

# Export as R Markdown (includes reproducible R code)
export_protocol_language(design, "protocol.Rmd")
} # }
```
