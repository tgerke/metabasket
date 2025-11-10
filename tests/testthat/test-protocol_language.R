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
