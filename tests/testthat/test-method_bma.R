test_that("BMA analysis works with basic data", {
  safe_safe_skip_if_not_installed("bmabasket")
  
  # Create simple basket trial data
  data <- basket_data(
    basket_names = paste0("Basket", 1:3),
    n_patients = c(25, 25, 25),
    n_responses = c(10, 8, 12)
  )
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 25,
    null_response_rates = 0.2,
    design_type = "bma"
  )
  
  result <- analyze_basket(data, design)
  
  # Check result structure
  expect_s3_class(result, "bma_result")
  expect_true("posterior_probs" %in% names(result))
  expect_true("posterior_means" %in% names(result))
  expect_true("rejections" %in% names(result))
  expect_true("basket_names" %in% names(result))
  
  # Check dimensions
  expect_length(result$posterior_probs, 3)
  expect_length(result$posterior_means, 3)
  expect_length(result$rejections, 3)
  
  # Check value ranges
  expect_true(all(result$posterior_probs >= 0 & result$posterior_probs <= 1))
  expect_true(all(result$posterior_means >= 0 & result$posterior_means <= 1))
  expect_type(result$rejections, "logical")
})


test_that("BMA analysis respects custom prior parameters", {
  safe_skip_if_not_installed("bmabasket")
  
  data <- basket_data(
    basket_names = c("A", "B"),
    n_patients = c(20, 20),
    n_responses = c(8, 10)
  )
  
  design <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    null_response_rates = 0.3,
    design_type = "bma",
    design_params = list(
      mu0 = 0.4,
      phi0 = 2,
      pmp0 = 0.5,
      post_prob_threshold = 0.90
    )
  )
  
  result <- analyze_basket(data, design)
  
  # Check that parameters were recorded
  expect_equal(result$design_params$mu0, 0.4)
  expect_equal(result$design_params$phi0, 2)
  expect_equal(result$design_params$pmp0, 0.5)
  expect_equal(result$threshold, 0.90)
})


test_that("extract_rejections works for BMA results", {
  safe_skip_if_not_installed("bmabasket")
  
  data <- basket_data(
    basket_names = c("X", "Y", "Z"),
    n_patients = c(30, 30, 30),
    n_responses = c(15, 12, 18)
  )
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 30,
    null_response_rates = 0.25,
    design_type = "bma"
  )
  
  result <- analyze_basket(data, design)
  rejections <- extract_rejections(result)
  
  # Check structure
  expect_type(rejections, "logical")
  expect_length(rejections, 3)
  
  # Should match result$rejections
  expect_equal(rejections, result$rejections)
})


test_that("print method works for BMA results", {
  safe_skip_if_not_installed("bmabasket")
  
  data <- basket_data(
    basket_names = c("Test1", "Test2"),
    n_patients = c(25, 25),
    n_responses = c(10, 8)
  )
  
  design <- basket_design(
    n_baskets = 2,
    sample_sizes = 25,
    null_response_rates = 0.2,
    design_type = "bma"
  )
  
  result <- analyze_basket(data, design)
  
  # Test that print doesn't error
  expect_output(print(result), "Bayesian Model Averaging")
  expect_output(print(result), "Prior parameters")
  expect_output(print(result), "mu0")
  expect_output(print(result), "Decision threshold")
})


test_that("BMA analysis validates inputs", {
  safe_skip_if_not_installed("bmabasket")
  
  data <- basket_data(
    basket_names = c("A", "B"),
    n_patients = c(20, 20),
    n_responses = c(8, 10)
  )
  
  # Wrong design type - call .bma method directly
  wrong_design <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    null_response_rates = 0.3,
    design_type = "cunanan"  # Not BMA
  )
  
  expect_error(
    analyze_basket.bma(data, wrong_design),
    "design_type = 'bma'"
  )
  
  # Not a basket_data object
  bma_design <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    null_response_rates = 0.3,
    design_type = "bma"
  )
  
  expect_error(
    analyze_basket.bma(list(x = 1), bma_design),
    "basket_data object"
  )
})


test_that("BMA analysis handles different threshold values", {
  safe_skip_if_not_installed("bmabasket")
  
  data <- basket_data(
    basket_names = c("A", "B", "C"),
    n_patients = c(25, 25, 25),
    n_responses = c(12, 10, 15)
  )
  
  # Low threshold (more rejections)
  design_low <- basket_design(
    n_baskets = 3,
    sample_sizes = 25,
    null_response_rates = 0.2,
    design_type = "bma",
    design_params = list(post_prob_threshold = 0.80)
  )
  
  # High threshold (fewer rejections)
  design_high <- basket_design(
    n_baskets = 3,
    sample_sizes = 25,
    null_response_rates = 0.2,
    design_type = "bma",
    design_params = list(post_prob_threshold = 0.99)
  )
  
  result_low <- analyze_basket(data, design_low)
  result_high <- analyze_basket(data, design_high)
  
  # Lower threshold should have >= rejections than higher threshold
  expect_true(sum(result_low$rejections) >= sum(result_high$rejections))
})


test_that("BMA handles vemurafenib trial data", {
  safe_skip_if_not_installed("bmabasket")
  
  data(vemurafenib_trial)
  
  # vemurafenib_trial is already a basket_data object
  n_baskets <- length(vemurafenib_trial$basket_names)
  
  # Create design for vemurafenib trial
  design <- basket_design(
    n_baskets = n_baskets,
    sample_sizes = rep(20, n_baskets),  # Simplified for testing
    null_response_rates = 0.15,
    design_type = "bma"
  )
  
  result <- analyze_basket(vemurafenib_trial, design)
  
  # Should run without error
  expect_s3_class(result, "bma_result")
  expect_length(result$posterior_probs, n_baskets)
  
  # All values should be valid
  expect_false(any(is.na(result$posterior_probs)))
  expect_false(any(is.na(result$posterior_means)))
})
