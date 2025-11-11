test_that("MEM analysis works with basic data", {
  safe_skip_if_not_installed("basket")
  
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
    design_type = "mem"
  )
  
  result <- analyze_basket(data, design)
  
  # Check result structure
  expect_s3_class(result, "mem_result")
  expect_true("posterior_probs" %in% names(result))
  expect_true("posterior_means" %in% names(result))
  expect_true("posterior_medians" %in% names(result))
  expect_true("hpd_intervals" %in% names(result))
  expect_true("rejections" %in% names(result))
  expect_true("basket_names" %in% names(result))
  expect_true("exchangeability_matrix" %in% names(result))
  
  # Check dimensions
  expect_length(result$posterior_probs, 3)
  expect_length(result$posterior_means, 3)
  expect_length(result$posterior_medians, 3)
  expect_length(result$rejections, 3)
  expect_equal(dim(result$hpd_intervals), c(2, 3))
  
  # Check value ranges
  expect_true(all(result$posterior_probs >= 0 & result$posterior_probs <= 1))
  expect_true(all(result$posterior_means >= 0 & result$posterior_means <= 1))
  expect_true(all(result$posterior_medians >= 0 & result$posterior_medians <= 1))
  expect_type(result$rejections, "logical")
})


test_that("MEM analysis respects custom prior parameters", {
  safe_skip_if_not_installed("basket")
  
  data <- basket_data(
    basket_names = c("A", "B"),
    n_patients = c(20, 20),
    n_responses = c(8, 10)
  )
  
  design <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    null_response_rates = 0.3,
    design_type = "mem",
    design_params = list(
      shape1 = 1,
      shape2 = 1,
      hpd_alpha = 0.10
    )
  )
  
  result <- analyze_basket(data, design)
  
  # Check that parameters were recorded
  expect_equal(result$design_params$shape1, 1)
  expect_equal(result$design_params$shape2, 1)
  expect_equal(result$design_params$hpd_alpha, 0.10)
  expect_equal(result$threshold, 0.90)
})


test_that("extract_rejections works for MEM results", {
  safe_skip_if_not_installed("basket")
  
  data <- basket_data(
    basket_names = c("X", "Y", "Z"),
    n_patients = c(30, 30, 30),
    n_responses = c(15, 12, 18)
  )
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 30,
    null_response_rates = 0.25,
    design_type = "mem"
  )
  
  result <- analyze_basket(data, design)
  rejections <- extract_rejections(result)
  
  # Check structure
  expect_type(rejections, "logical")
  expect_length(rejections, 3)
  
  # Should match result$rejections
  expect_equal(rejections, result$rejections)
})


test_that("print method works for MEM results", {
  safe_skip_if_not_installed("basket")
  
  data <- basket_data(
    basket_names = c("Test1", "Test2"),
    n_patients = c(25, 25),
    n_responses = c(10, 8)
  )
  
  design <- basket_design(
    n_baskets = 2,
    sample_sizes = 25,
    null_response_rates = 0.2,
    design_type = "mem"
  )
  
  result <- analyze_basket(data, design)
  
  # Test that print doesn't error
  expect_output(print(result), "Multi-source Exchangeability Model")
  expect_output(print(result), "Prior parameters")
  expect_output(print(result), "shape1")
  expect_output(print(result), "Decision threshold")
  expect_output(print(result), "Exchangeability")
})


test_that("MEM analysis validates inputs", {
  safe_skip_if_not_installed("basket")
  
  data <- basket_data(
    basket_names = c("A", "B"),
    n_patients = c(20, 20),
    n_responses = c(8, 10)
  )
  
  # Wrong design type - call .mem method directly
  wrong_design <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    null_response_rates = 0.3,
    design_type = "cunanan"  # Not MEM
  )
  
  expect_error(
    analyze_basket.mem(data, wrong_design),
    "design_type = 'mem'"
  )
  
  # Not a basket_data object
  mem_design <- basket_design(
    n_baskets = 2,
    sample_sizes = 20,
    null_response_rates = 0.3,
    design_type = "mem"
  )
  
  expect_error(
    analyze_basket.mem(list(x = 1), mem_design),
    "basket_data object"
  )
})


test_that("MEM analysis handles different threshold values", {
  safe_skip_if_not_installed("basket")
  
  data <- basket_data(
    basket_names = c("A", "B", "C"),
    n_patients = c(25, 25, 25),
    n_responses = c(12, 10, 15)
  )
  
  # Lower alpha (higher threshold for rejection)
  design_low <- basket_design(
    n_baskets = 3,
    sample_sizes = 25,
    null_response_rates = 0.2,
    design_type = "mem",
    design_params = list(hpd_alpha = 0.01)  # 99% threshold
  )
  
  # Higher alpha (lower threshold for rejection)
  design_high <- basket_design(
    n_baskets = 3,
    sample_sizes = 25,
    null_response_rates = 0.2,
    design_type = "mem",
    design_params = list(hpd_alpha = 0.10)  # 90% threshold
  )
  
  result_low <- analyze_basket(data, design_low)
  result_high <- analyze_basket(data, design_high)
  
  # Lower alpha (more stringent) should have <= rejections than higher alpha
  expect_true(sum(result_low$rejections) <= sum(result_high$rejections))
})


test_that("MEM handles vemurafenib trial data", {
  safe_skip_if_not_installed("basket")
  
  data(vemurafenib_trial)
  
  # vemurafenib_trial is already a basket_data object
  n_baskets <- length(vemurafenib_trial$basket_names)
  
  # Create design for vemurafenib trial
  design <- basket_design(
    n_baskets = n_baskets,
    sample_sizes = rep(20, n_baskets),  # Simplified for testing
    null_response_rates = 0.15,
    design_type = "mem"
  )
  
  result <- analyze_basket(vemurafenib_trial, design)
  
  # Should run without error
  expect_s3_class(result, "mem_result")
  expect_length(result$posterior_probs, n_baskets)
  
  # All values should be valid
  expect_false(any(is.na(result$posterior_probs)))
  expect_false(any(is.na(result$posterior_means)))
  expect_false(any(is.na(result$posterior_medians)))
})


test_that("MEM exchangeability matrix is computed", {
  safe_skip_if_not_installed("basket")
  
  data <- basket_data(
    basket_names = c("A", "B", "C"),
    n_patients = c(20, 20, 20),
    n_responses = c(10, 10, 2)  # A and B similar, C different
  )
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    null_response_rates = 0.2,
    design_type = "mem"
  )
  
  result <- analyze_basket(data, design)
  
  # Exchangeability matrix should exist
  expect_false(is.null(result$exchangeability_matrix))
  expect_equal(dim(result$exchangeability_matrix), c(3, 3))
  
  # Diagonal should be 1 (basket always exchangeable with itself)
  expect_equal(as.numeric(diag(result$exchangeability_matrix)), c(1, 1, 1))
  
  # Upper triangle should have values, lower triangle NA (per basket package structure)
  expect_true(!is.na(result$exchangeability_matrix[1, 2]))
  expect_true(is.na(result$exchangeability_matrix[2, 1]))
})


test_that("MEM handles custom prior matrix", {
  safe_skip_if_not_installed("basket")
  
  data <- basket_data(
    basket_names = c("A", "B", "C"),
    n_patients = c(20, 20, 20),
    n_responses = c(8, 10, 6)
  )
  
  # Custom prior: A and B likely exchangeable, C separate
  custom_prior <- matrix(c(
    1.0, 0.8, 0.2,
    0.8, 1.0, 0.2,
    0.2, 0.2, 1.0
  ), nrow = 3, byrow = TRUE)
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    null_response_rates = 0.2,
    design_type = "mem",
    design_params = list(
      prior = custom_prior
    )
  )
  
  result <- analyze_basket(data, design)
  
  # Should run without error
  expect_s3_class(result, "mem_result")
  
  # Check prior was used
  expect_equal(result$design_params$prior, custom_prior)
})
