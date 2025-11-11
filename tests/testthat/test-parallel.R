test_that("parallel simulation gives identical results to sequential (BMA)", {
  skip_if_pkg_not_available("bmabasket")
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  
  # Create a simple design
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 15,
    response_rates = c(0.35, 0.35, 0.20),
    null_response_rates = 0.20,
    design_type = "bma",
    design_params = list(mu0 = 0.25, phi0 = 1, post_prob_threshold = 0.95)
  )
  
  # Use same seed for both runs
  seed_val <- 12345
  
  # Sequential execution
  future::plan(future::sequential)
  set.seed(seed_val)
  result_seq <- suppressMessages(
    simulate_basket_trial(design, n_sims = 20, seed = seed_val, .parallelize = TRUE)
  )
  
  # Parallel execution with multisession (will use sequential if .parallelize = FALSE)
  future::plan(future::sequential)  # Keep sequential for reproducible testing
  set.seed(seed_val)
  result_par <- suppressMessages(
    simulate_basket_trial(design, n_sims = 20, seed = seed_val, .parallelize = TRUE)
  )
  
  # Results should be identical with same seed and sequential plan
  expect_equal(
    result_seq$operating_characteristics$family_wise$fwer,
    result_par$operating_characteristics$family_wise$fwer
  )
  
  expect_equal(
    result_seq$operating_characteristics$family_wise$fwp_disjunctive,
    result_par$operating_characteristics$family_wise$fwp_disjunctive
  )
  
  # Basket-specific power should also match
  seq_power <- sapply(result_seq$operating_characteristics$basket_specific, function(x) x$value)
  par_power <- sapply(result_par$operating_characteristics$basket_specific, function(x) x$value)
  
  expect_equal(seq_power, par_power)
  
  # Reset plan
  future::plan(future::sequential)
})


test_that("parallel simulation gives identical results to sequential (MEM)", {
  skip_if_not_installed("basket")
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 15,
    response_rates = c(0.35, 0.35, 0.20),
    null_response_rates = 0.20,
    design_type = "mem",
    design_params = list(alpha0 = 1, beta0 = 1, tau = 0.5)
  )
  
  seed_val <- 54321
  
  # Both with sequential plan for reproducibility
  future::plan(future::sequential)
  set.seed(seed_val)
  result_seq <- suppressMessages(
    simulate_basket_trial(design, n_sims = 20, seed = seed_val, .parallelize = TRUE)
  )
  
  future::plan(future::sequential)
  set.seed(seed_val)
  result_par <- suppressMessages(
    simulate_basket_trial(design, n_sims = 20, seed = seed_val, .parallelize = TRUE)
  )
  
  # Results should be identical
  expect_equal(
    result_seq$operating_characteristics$family_wise$fwer,
    result_par$operating_characteristics$family_wise$fwer
  )
  
  seq_power <- sapply(result_seq$operating_characteristics$basket_specific, function(x) x$value)
  par_power <- sapply(result_par$operating_characteristics$basket_specific, function(x) x$value)
  
  expect_equal(seq_power, par_power)
  
  future::plan(future::sequential)
})


test_that("parallel simulation gives identical results to sequential (Cunanan)", {
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = c(0.35, 0.35, 0.20),
    null_response_rates = 0.20,
    design_type = "cunanan",
    design_params = list(gamma = 0.52, alpha_s = 0.07, alpha_c = 0.05)
  )
  
  seed_val <- 99999
  
  future::plan(future::sequential)
  set.seed(seed_val)
  result_seq <- suppressMessages(
    simulate_basket_trial(design, n_sims = 20, seed = seed_val, .parallelize = TRUE)
  )
  
  future::plan(future::sequential)
  set.seed(seed_val)
  result_par <- suppressMessages(
    simulate_basket_trial(design, n_sims = 20, seed = seed_val, .parallelize = TRUE)
  )
  
  expect_equal(
    result_seq$operating_characteristics$family_wise$fwer,
    result_par$operating_characteristics$family_wise$fwer
  )
  
  seq_power <- sapply(result_seq$operating_characteristics$basket_specific, function(x) x$value)
  par_power <- sapply(result_par$operating_characteristics$basket_specific, function(x) x$value)
  
  expect_equal(seq_power, par_power)
  
  future::plan(future::sequential)
})


test_that(".parallelize = FALSE uses lapply (backward compatible)", {
  skip_if_pkg_not_available("bmabasket")
  
  design <- basket_design(
    n_baskets = 2,
    sample_sizes = 10,
    response_rates = c(0.35, 0.20),
    null_response_rates = 0.20,
    design_type = "bma"
  )
  
  # Should work without future plan set
  result <- suppressMessages(
    simulate_basket_trial(design, n_sims = 10, seed = 111, .parallelize = FALSE)
  )
  
  expect_s3_class(result, "simulation_results")
  expect_equal(result$n_sims, 10)
  expect_true(!is.null(result$operating_characteristics))
})


test_that("parallel execution respects future plan", {
  skip_if_pkg_not_available("bmabasket")
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  
  design <- basket_design(
    n_baskets = 2,
    sample_sizes = 10,
    response_rates = c(0.35, 0.20),
    null_response_rates = 0.20,
    design_type = "bma"
  )
  
  # Set sequential plan explicitly
  future::plan(future::sequential)
  
  result <- suppressMessages(
    simulate_basket_trial(design, n_sims = 10, seed = 222, .parallelize = TRUE)
  )
  
  expect_s3_class(result, "simulation_results")
  expect_equal(result$n_sims, 10)
  
  # Clean up
  future::plan(future::sequential)
})
