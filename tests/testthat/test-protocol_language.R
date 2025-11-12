test_that("generate_protocol_language produces valid output", {
  
  design <- basket_design(
    n_baskets = 4,
    basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
    sample_sizes = 25,
    response_rates = 0.35,
    null_response_rates = 0.20,
    design_type = "bma"
  )
  
  protocol_text <- generate_protocol_language(design)
  
  expect_type(protocol_text, "character")
  expect_gt(nchar(protocol_text), 100)
  
  # Check that key sections are present
  expect_match(protocol_text, "Basket Trial Design")
  expect_match(protocol_text, "Trial Overview")
  expect_match(protocol_text, "Statistical Methodology")
  expect_match(protocol_text, "Hypothesis Testing")
  expect_match(protocol_text, "Decision Criteria")
  expect_match(protocol_text, "Operating Characteristics")
  
  # Check that design details are included
  expect_match(protocol_text, "4 cancer types")
  expect_match(protocol_text, "NSCLC")
  expect_match(protocol_text, "25 patients")
})


test_that("generate_protocol_language includes method-specific content", {
  
  # Test BMA
  design_bma <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    response_rates = 0.30,
    null_response_rates = 0.15,
    design_type = "bma"
  )
  
  text_bma <- generate_protocol_language(design_bma)
  expect_match(text_bma, "Bayesian Model Averaging")
  expect_match(text_bma, "partitions")
  
  # Test MEM
  design_mem <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    response_rates = 0.30,
    null_response_rates = 0.15,
    design_type = "mem"
  )
  
  text_mem <- generate_protocol_language(design_mem)
  expect_match(text_mem, "Multi-source Exchangeability")
  expect_match(text_mem, "exchangeability")
  
  # Test BHM
  design_bhm <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    response_rates = 0.30,
    null_response_rates = 0.15,
    design_type = "bhm"
  )
  
  text_bhm <- generate_protocol_language(design_bhm)
  expect_match(text_bhm, "Bayesian Hierarchical Model")
  expect_match(text_bhm, "shrinkage")
})


test_that("generate_protocol_language handles references correctly", {
  
  design <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    response_rates = 0.30,
    null_response_rates = 0.15,
    design_type = "bhm"
  )
  
  # With references
  text_with_refs <- generate_protocol_language(design, include_references = TRUE)
  expect_match(text_with_refs, "References")
  expect_match(text_with_refs, "Berry")
  
  # Without references
  text_no_refs <- generate_protocol_language(design, include_references = FALSE)
  expect_false(grepl("References", text_no_refs))
})


test_that("generate_protocol_language handles technical details", {
  
  design <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    response_rates = 0.30,
    null_response_rates = 0.15,
    design_type = "bma"
  )
  
  # With details
  text_with_details <- generate_protocol_language(
    design, 
    include_statistical_details = TRUE
  )
  expect_match(text_with_details, "Technical Details")
  
  # Without details
  text_no_details <- generate_protocol_language(
    design, 
    include_statistical_details = FALSE
  )
  expect_false(grepl("Technical Details", text_no_details))
})


test_that("generate_protocol_language works for single Simon design", {
  skip_if_pkg_not_available("clinfun")
  
  design <- simon_design(
    n_baskets = 1,
    basket_names = "NSCLC",
    sample_sizes = 43,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40,
    alpha = 0.10,
    beta = 0.20,
    design_type = "optimal"
  )
  
  protocol_text <- generate_protocol_language(design)
  
  expect_type(protocol_text, "character")
  expect_gt(nchar(protocol_text), 100)
  
  # Check key sections for single Simon design
  expect_match(protocol_text, "Simon Two-Stage Design")
  expect_match(protocol_text, "gold standard")
  expect_match(protocol_text, "Trial Overview")
  expect_match(protocol_text, "Trial Population")
  expect_match(protocol_text, "Statistical Methodology")
  expect_match(protocol_text, "Decision Criteria")
  
  # Check for detailed decision criteria language
  expect_match(protocol_text, "Stage 1 Enrollment")
  expect_match(protocol_text, "Stage 2 Enrollment")
  expect_match(protocol_text, "\\(r1 \\+ 1\\) or more")
  expect_match(protocol_text, "\\(r \\+ 1\\) or more")
  expect_match(protocol_text, "accrual will be held")
  
  # Check it mentions improvement from null to alternative
  expect_match(protocol_text, "20.0%")
  expect_match(protocol_text, "40.0%")
  
  # Single design should NOT mention Bonferroni
  expect_false(grepl("Bonferroni", protocol_text))
  
  # Should mention Type I error control for single design
  expect_match(protocol_text, "Type I Error Control")
  expect_match(protocol_text, "alpha = 0.100")
})


test_that("generate_protocol_language works for multi-cohort Simon design", {
  skip_if_pkg_not_available("clinfun")
  
  design <- simon_design(
    n_baskets = 4,
    basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
    sample_sizes = 25,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40,
    alpha = 0.0125,  # Bonferroni-adjusted
    beta = 0.20,
    design_type = "optimal"
  )
  
  protocol_text <- generate_protocol_language(design)
  
  expect_type(protocol_text, "character")
  expect_gt(nchar(protocol_text), 100)
  
  # Check key sections for multi-cohort design
  expect_match(protocol_text, "Multi-Cohort Trial")
  expect_match(protocol_text, "Trial Cohorts")
  expect_match(protocol_text, "independent")
  expect_match(protocol_text, "4 cancer types")
  
  # Multi-cohort with small alpha should mention Bonferroni
  expect_match(protocol_text, "Bonferroni")
  expect_match(protocol_text, "Multiple Testing")
  
  # Should list all cohorts
  expect_match(protocol_text, "NSCLC")
  expect_match(protocol_text, "SCLC")
  expect_match(protocol_text, "Melanoma")
  expect_match(protocol_text, "RCC")
})


test_that("generate_protocol_language for Simon handles references", {
  skip_if_pkg_not_available("clinfun")
  
  design <- simon_design(
    n_baskets = 1,
    basket_names = "Test",
    sample_sizes = 30,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40,
    alpha = 0.10,
    beta = 0.20,
    design_type = "optimal"
  )
  
  # With references
  text_with_refs <- generate_protocol_language(design, include_references = TRUE)
  expect_match(text_with_refs, "References")
  expect_match(text_with_refs, "Simon R")
  expect_match(text_with_refs, "1989")
  
  # Without references
  text_no_refs <- generate_protocol_language(design, include_references = FALSE)
  expect_false(grepl("References", text_no_refs))
})


test_that("generate_protocol_language for Simon detects Bonferroni correctly", {
  skip_if_pkg_not_available("clinfun")
  
  # Large alpha - should suggest Bonferroni
  design_large_alpha <- simon_design(
    n_baskets = 4,
    basket_names = c("A", "B", "C", "D"),
    sample_sizes = 25,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40,
    alpha = 0.05,  # NOT Bonferroni-adjusted
    beta = 0.20,
    design_type = "optimal"
  )
  
  text_large <- generate_protocol_language(design_large_alpha)
  expect_match(text_large, "family-wise error rate")
  expect_match(text_large, "consider using")
  
  # Small alpha - should say Bonferroni applied
  design_small_alpha <- simon_design(
    n_baskets = 4,
    basket_names = c("A", "B", "C", "D"),
    sample_sizes = 25,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40,
    alpha = 0.0125,  # Bonferroni-adjusted (0.05/4)
    beta = 0.20,
    design_type = "optimal"
  )
  
  text_small <- generate_protocol_language(design_small_alpha)
  expect_match(text_small, "Bonferroni correction is applied")
})
