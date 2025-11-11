test_that("BHM method requires bhmbasket package", {
  # Check if bhmbasket is available
  has_bhmbasket <- requireNamespace("bhmbasket", quietly = TRUE)
  
  # This test only runs if bhmbasket is NOT installed
  skip_if(has_bhmbasket, "bhmbasket is installed")
  
  # Create simple test data
  data_bhm <- basket_data(
    n_patients = c(20, 20, 20),
    n_responses = c(8, 10, 12),
    basket_names = c("A", "B", "C")
  )
  
  design_bhm <- basket_design(
    design_type = "bhm",
    n_baskets = 3,
    sample_sizes = 20,
    null_response_rates = rep(0.2, 3)
  )
  
  expect_error(
    analyze_basket(data_bhm, design_bhm),
    "bhmbasket.*required"
  )
})

test_that("BHM analysis works with JAGS installed", {
  safe_skip_if_not_installed("bhmbasket")
  skip_if_not(tryCatch({
    requireNamespace("rjags", quietly = TRUE)
  }, error = function(e) FALSE), "JAGS not installed")
  
  # Create test data
  data_bhm <- basket_data(
    n_patients = c(20, 20, 20),
    n_responses = c(8, 10, 12),
    basket_names = c("A", "B", "C")
  )
  
  design_bhm <- basket_design(
    design_type = "bhm",
    n_baskets = 3,
    sample_sizes = 20,
    null_response_rates = rep(0.2, 3),
    design_params = list(
      method = "berry",
      n_mcmc_iterations = 1000,
      evidence_level = 0.1
    )
  )
  
  # Run analysis
  result <- analyze_basket(data_bhm, design_bhm)
  
  # Check result structure
  expect_s3_class(result, "bhm_result")
  expect_s3_class(result, "basket_result")
  expect_true(is.list(result))
  
  # Check required components
  expect_true("posterior_means" %in% names(result))
  expect_true("posterior_probs" %in% names(result))
  expect_true("rejections" %in% names(result))
  expect_true("basket_names" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("threshold" %in% names(result))
  
  # Check dimensions
  expect_length(result$posterior_means, 3)
  expect_length(result$posterior_probs, 3)
  expect_length(result$rejections, 3)
  
  # Check value ranges
  expect_true(all(result$posterior_means >= 0 & result$posterior_means <= 1))
  expect_true(all(result$posterior_probs >= 0 & result$posterior_probs <= 1))
  expect_type(result$rejections, "logical")
  
  # Check method
  expect_equal(result$method, "berry")
})

test_that("BHM works with different methods", {
  safe_skip_if_not_installed("bhmbasket")
  skip_if_not(tryCatch({
    requireNamespace("rjags", quietly = TRUE)
  }, error = function(e) FALSE), "JAGS not installed")
  
  data_bhm <- basket_data(
    n_patients = c(15, 15, 15),
    n_responses = c(5, 6, 7),
    basket_names = c("A", "B", "C")
  )
  
  # Test exnex method
  design_exnex <- basket_design(
    design_type = "bhm",
    n_baskets = 3,
    sample_sizes = 15,
    null_response_rates = rep(0.2, 3),
    design_params = list(
      method = "exnex",
      n_mcmc_iterations = 1000,
      evidence_level = 0.05
    )
  )
  
  result_exnex <- analyze_basket(data_bhm, design_exnex)
  expect_s3_class(result_exnex, "bhm_result")
  expect_equal(result_exnex$method, "exnex")
  
  # Test pooled method
  design_pooled <- basket_design(
    design_type = "bhm",
    n_baskets = 3,
    sample_sizes = 15,
    null_response_rates = rep(0.2, 3),
    design_params = list(
      method = "pooled",
      n_mcmc_iterations = 1000
    )
  )
  
  result_pooled <- analyze_basket(data_bhm, design_pooled)
  expect_s3_class(result_pooled, "bhm_result")
  expect_equal(result_pooled$method, "pooled")
})

test_that("BHM rejects invalid methods", {
  safe_skip_if_not_installed("bhmbasket")
  skip_if_not(tryCatch({
    requireNamespace("rjags", quietly = TRUE)
  }, error = function(e) FALSE), "JAGS not installed")
  
  data_bhm <- basket_data(
    n_patients = c(20, 20),
    n_responses = c(8, 10),
    basket_names = c("A", "B")
  )
  
  design_invalid <- basket_design(
    design_type = "bhm",
    n_baskets = 2,
    sample_sizes = 20,
    null_response_rates = rep(0.2, 2),
    design_params = list(method = "invalid_method")
  )
  
  expect_error(
    analyze_basket(data_bhm, design_invalid),
    "method must be one of"
  )
})

test_that("BHM print method works", {
  safe_skip_if_not_installed("bhmbasket")
  skip_if_not(tryCatch({
    requireNamespace("rjags", quietly = TRUE)
  }, error = function(e) FALSE), "JAGS not installed")
  
  data_bhm <- basket_data(
    n_patients = c(20, 20, 20),
    n_responses = c(8, 10, 12),
    basket_names = c("Basket A", "Basket B", "Basket C")
  )
  
  design_bhm <- basket_design(
    design_type = "bhm",
    n_baskets = 3,
    sample_sizes = 20,
    null_response_rates = rep(0.2, 3),
    design_params = list(n_mcmc_iterations = 1000)
  )
  
  result <- analyze_basket(data_bhm, design_bhm)
  
  # Capture output
  output <- capture.output(print(result))
  
  # Check key elements are present
  expect_true(any(grepl("Bayesian Hierarchical Model", output)))
  expect_true(any(grepl("Method:", output)))
  expect_true(any(grepl("MCMC iterations:", output)))
  expect_true(any(grepl("Basket A", output)))
  expect_true(any(grepl("Post_Mean", output)))
  expect_true(any(grepl("Post_Prob", output)))
  expect_true(any(grepl("Reject_H0", output)))
})

test_that("extract_rejections works for BHM", {
  safe_skip_if_not_installed("bhmbasket")
  skip_if_not(tryCatch({
    requireNamespace("rjags", quietly = TRUE)
  }, error = function(e) FALSE), "JAGS not installed")
  
  data_bhm <- basket_data(
    n_patients = c(20, 20, 20),
    n_responses = c(8, 10, 12),
    basket_names = c("A", "B", "C")
  )
  
  design_bhm <- basket_design(
    design_type = "bhm",
    n_baskets = 3,
    sample_sizes = 20,
    null_response_rates = rep(0.2, 3),
    design_params = list(n_mcmc_iterations = 1000)
  )
  
  result <- analyze_basket(data_bhm, design_bhm)
  rejections <- extract_rejections(result)
  
  expect_type(rejections, "logical")
  expect_length(rejections, 3)
  expect_true(all(!is.na(rejections)))
})

test_that("BHM works with vemurafenib data", {
  safe_skip_if_not_installed("bhmbasket")
  skip_if_not(tryCatch({
    requireNamespace("rjags", quietly = TRUE)
  }, error = function(e) FALSE), "JAGS not installed")
  
  data("vemurafenib_trial", package = "metabasket")
  
  # Create basket data from vemurafenib_trial
  data_vem <- basket_data(
    n_patients = vemurafenib_trial$n_patients,
    n_responses = vemurafenib_trial$n_responses,
    basket_names = vemurafenib_trial$basket_names
  )
  
  design_bhm <- basket_design(
    design_type = "bhm",
    n_baskets = length(vemurafenib_trial$basket_names),
    sample_sizes = vemurafenib_trial$n_patients,
    null_response_rates = rep(0.15, length(vemurafenib_trial$basket_names)),
    design_params = list(
      method = "berry",
      n_mcmc_iterations = 1000,
      evidence_level = 0.05
    )
  )
  
  result <- analyze_basket(data_vem, design_bhm)
  
  expect_s3_class(result, "bhm_result")
  expect_length(result$posterior_means, length(vemurafenib_trial$basket_names))
  expect_length(result$rejections, length(vemurafenib_trial$basket_names))
  
  # Check that high response baskets tend to have higher posterior probs
  response_rates <- vemurafenib_trial$n_responses / vemurafenib_trial$n_patients
  high_response <- response_rates > 0.3
  if (any(high_response)) {
    # At least one high-response basket should have decent posterior prob
    expect_true(any(result$posterior_probs[high_response] > 0.5))
  }
})

test_that("BHM handles different evidence levels", {
  safe_skip_if_not_installed("bhmbasket")
  skip_if_not(tryCatch({
    requireNamespace("rjags", quietly = TRUE)
  }, error = function(e) FALSE), "JAGS not installed")
  
  data_bhm <- basket_data(
    n_patients = c(20, 20, 20),
    n_responses = c(12, 13, 14),
    basket_names = c("A", "B", "C")
  )
  
  # More stringent threshold (evidence_level = 0.01)
  design_strict <- basket_design(
    design_type = "bhm",
    n_baskets = 3,
    sample_sizes = 20,
    null_response_rates = rep(0.3, 3),
    design_params = list(
      method = "berry",
      n_mcmc_iterations = 1000,
      evidence_level = 0.01
    )
  )
  
  result_strict <- analyze_basket(data_bhm, design_strict)
  
  # Lenient threshold (evidence_level = 0.2)
  design_lenient <- basket_design(
    design_type = "bhm",
    n_baskets = 3,
    sample_sizes = 20,
    null_response_rates = rep(0.3, 3),
    design_params = list(
      method = "berry",
      n_mcmc_iterations = 1000,
      evidence_level = 0.2
    )
  )
  
  result_lenient <- analyze_basket(data_bhm, design_lenient)
  
  # Lenient threshold should reject at least as many
  expect_true(sum(result_lenient$rejections) >= sum(result_strict$rejections))
  
  # Check threshold values
  expect_equal(result_strict$threshold, 0.99)
  expect_equal(result_lenient$threshold, 0.80)
})

test_that("BHM default parameters are applied correctly", {
  safe_skip_if_not_installed("bhmbasket")
  skip_if_not(tryCatch({
    requireNamespace("rjags", quietly = TRUE)
  }, error = function(e) FALSE), "JAGS not installed")
  
  data_bhm <- basket_data(
    n_patients = c(20, 20),
    n_responses = c(8, 10),
    basket_names = c("A", "B")
  )
  
  # Design with no parameters (should use defaults)
  design_default <- basket_design(
    design_type = "bhm",
    n_baskets = 2,
    sample_sizes = 20,
    null_response_rates = rep(0.2, 2)
  )
  
  result <- analyze_basket(data_bhm, design_default)
  
  # Check defaults were applied
  expect_equal(result$method, "berry")
  expect_equal(result$n_mcmc_iterations, 10000)
  expect_equal(result$evidence_level, 0.1)
  expect_equal(result$threshold, 0.9)
})
