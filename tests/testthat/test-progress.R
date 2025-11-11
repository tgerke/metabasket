test_that("Progress reporting works without errors", {
  safe_skip_if_not_installed("bmabasket")
  
  # Test that simulations complete successfully with progressr enabled
  # (progressr will be silent unless handlers are explicitly set)
  
  design <- basket_design(
    design_type = "bma",
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = 0.4,
    null_response_rates = 0.2
  )
  
  # Progress reporting is built-in but silent unless handlers are set
  # This just verifies no errors are introduced
  expect_no_error({
    results <- simulate_basket_trial(
      design, 
      n_sims = 5, 
      seed = 42
    )
  })
  
  expect_s3_class(results, "simulation_results")
})

test_that("Progress reporting works with parallel execution", {
  skip_on_cran()
  safe_skip_if_not_installed("bmabasket")
  
  design <- basket_design(
    design_type = "bma",
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = 0.4,
    null_response_rates = 0.2
  )
  
  # Temporarily set up sequential plan for testing
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)
  
  # Progress reporting should work with parallel flag
  expect_no_error({
    results <- simulate_basket_trial(
      design, 
      n_sims = 5, 
      seed = 42,
      .parallelize = TRUE
    )
  })
  
  expect_s3_class(results, "simulation_results")
})
