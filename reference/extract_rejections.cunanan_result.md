# Extract rejection decisions from Cunanan analysis

Extract rejection decisions from Cunanan analysis

## Usage

``` r
# S3 method for class 'cunanan_result'
extract_rejections(result, ...)
```

## Arguments

- result:

  A `cunanan_result` object from
  [`analyze_basket.cunanan()`](https://tgerke.github.io/metabasket/reference/analyze_basket.cunanan.md)

- ...:

  Additional arguments (not currently used)

## Value

Logical vector indicating which baskets were rejected (declared active)
