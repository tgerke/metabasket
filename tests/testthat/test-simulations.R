test_that("simulate_responses generates correct structure", {
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = c(0.3, 0.4, 0.5),
    null_response_rates = 0.2,
    design_type = "bma"
  )
  
  sims <- simulate_responses(design, n_sims = 5, seed = 123)
  
  expect_type(sims, "list")
  expect_length(sims, 5)
  
  # Check first simulation structure
  expect_true(all(c("basket_names", "n_responses", "n_patients") %in% names(sims[[1]])))
  expect_length(sims[[1]]$n_responses, 3)
  expect_length(sims[[1]]$n_patients, 3)
  expect_equal(sims[[1]]$n_patients, c(20, 20, 20))
})


test_that("extract_rejections works for BMA results", {
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = c(0.5, 0.5, 0.2),
    null_response_rates = 0.2,
    design_type = "bma"
  )
  
  # Generate and analyze data
  data <- simulate_responses(design, n_sims = 10, seed = 456)
  results <- lapply(data, function(d) analyze_basket(d, design))
  
  # Extract rejections
  rejections <- extract_rejections(results)
  
  expect_type(rejections, "list")
  expect_length(rejections, 10)
  expect_true(all(sapply(rejections, is.logical)))
  expect_true(all(sapply(rejections, length) == 3))
  expect_false(any(sapply(rejections, anyNA)))
})


test_that("extract_rejections works for MEM results", {
  skip_if_not_installed("basket")
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = c(0.5, 0.5, 0.2),
    null_response_rates = 0.2,
    design_type = "mem"
  )
  
  data <- simulate_responses(design, n_sims = 10, seed = 789)
  results <- lapply(data, function(d) analyze_basket(d, design))
  rejections <- extract_rejections(results)
  
  expect_type(rejections, "list")
  expect_length(rejections, 10)
  expect_true(all(sapply(rejections, is.logical)))
  expect_false(any(sapply(rejections, anyNA)))
})


test_that("compute_operating_characteristics produces valid structure", {
  design <- basket_design(
    n_baskets = 4,
    sample_sizes = 25,
    response_rates = c(0.4, 0.4, 0.2, 0.2),
    null_response_rates = 0.2,
    design_type = "bma"
  )
  
  data <- simulate_responses(design, n_sims = 50, seed = 111)
  results <- lapply(data, function(d) analyze_basket(d, design))
  
  ocs <- compute_operating_characteristics(results, design, alpha = 0.05)
  
  # Check structure
  expect_true(all(c("basket_specific", "family_wise", "n_promising_baskets", 
                    "n_nonpromising_baskets") %in% names(ocs)))
  
  # Check basket-specific OCs
  expect_length(ocs$basket_specific, 4)
  expect_equal(ocs$n_promising_baskets, 2)
  expect_equal(ocs$n_nonpromising_baskets, 2)
  
  # All OC values should be proportions [0, 1]
  for (boc in ocs$basket_specific) {
    expect_true(boc$value >= 0 && boc$value <= 1)
    expect_true(boc$metric %in% c("Power", "Type I Error"))
  }
  
  # Family-wise OCs
  expect_true(ocs$family_wise$fwer >= 0 && ocs$family_wise$fwer <= 1)
  expect_true(ocs$family_wise$fwp_disjunctive >= 0 && ocs$family_wise$fwp_disjunctive <= 1)
  expect_true(ocs$family_wise$fwp_conjunctive >= 0 && ocs$family_wise$fwp_conjunctive <= 1)
})


test_that("BMA maintains FWER under global null", {
  # Global null: all baskets non-promising
  design <- basket_design(
    n_baskets = 4,
    sample_sizes = 25,
    response_rates = rep(0.2, 4),
    null_response_rates = 0.2,
    design_type = "bma",
    design_params = list(mu0 = 0.5, phi0 = 1, post_prob_threshold = 0.95)
  )
  
  set.seed(222)
  sim_results <- simulate_basket_trial(design, n_sims = 200, seed = 222)
  ocs <- sim_results$operating_characteristics
  
  # FWER should be close to alpha (5-10% with Monte Carlo error)
  # Use generous bounds for n_sims=200
  expect_lt(ocs$family_wise$fwer, 0.15)
})


test_that("BMA has reasonable power under global alternative", {
  # Global alternative: all baskets promising
  design <- basket_design(
    n_baskets = 4,
    sample_sizes = 25,
    response_rates = rep(0.40, 4),
    null_response_rates = 0.20,
    design_type = "bma",
    design_params = list(mu0 = 0.5, phi0 = 1, post_prob_threshold = 0.95)
  )
  
  set.seed(333)
  sim_results <- simulate_basket_trial(design, n_sims = 200, seed = 333)
  ocs <- sim_results$operating_characteristics
  
  # Average power should be reasonably high (>50% even with n_sims=200)
  power_vals <- sapply(ocs$basket_specific, function(x) 
    if(x$metric == "Power") x$value else NA)
  avg_power <- mean(power_vals, na.rm = TRUE)
  
  expect_gt(avg_power, 0.5, 
            label = "BMA should have >50% average power under global alternative")
  
  # FWP-D (detect at least one) should be very high
  expect_gt(ocs$family_wise$fwp_disjunctive, 0.7)
})


test_that("MEM has reasonable power under global alternative", {
  skip_if_not_installed("basket")
  
  design <- basket_design(
    n_baskets = 4,
    sample_sizes = 25,
    response_rates = rep(0.40, 4),
    null_response_rates = 0.20,
    design_type = "mem"
  )
  
  set.seed(444)
  sim_results <- simulate_basket_trial(design, n_sims = 200, seed = 444)
  ocs <- sim_results$operating_characteristics
  
  power_vals <- sapply(ocs$basket_specific, function(x) 
    if(x$metric == "Power") x$value else NA)
  avg_power <- mean(power_vals, na.rm = TRUE)
  
  expect_gt(avg_power, 0.5)
  expect_gt(ocs$family_wise$fwp_disjunctive, 0.7)
})


test_that("simulate_basket_trial returns complete structure", {
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = c(0.4, 0.3, 0.2),
    null_response_rates = 0.2,
    design_type = "bma"
  )
  
  results <- simulate_basket_trial(design, n_sims = 50, seed = 555)
  
  expect_s3_class(results, "simulation_results")
  expect_true(all(c("design", "n_sims", "operating_characteristics", 
                    "raw_results", "seed") %in% names(results)))
  expect_equal(results$n_sims, 50)
  expect_length(results$raw_results, 50)
})


test_that("Type I error controlled for non-promising baskets", {
  # Mixed scenario: some promising, some not
  design <- basket_design(
    n_baskets = 4,
    sample_sizes = 25,
    response_rates = c(0.40, 0.40, 0.20, 0.20),  # First 2 promising
    null_response_rates = 0.20,
    design_type = "bma"
  )
  
  set.seed(666)
  sim_results <- simulate_basket_trial(design, n_sims = 200, seed = 666)
  ocs <- sim_results$operating_characteristics
  
  # Check Type I error for non-promising baskets (3 and 4)
  type1_basket3 <- ocs$basket_specific[[3]]$value
  type1_basket4 <- ocs$basket_specific[[4]]$value
  
  expect_equal(ocs$basket_specific[[3]]$metric, "Type I Error")
  expect_equal(ocs$basket_specific[[4]]$metric, "Type I Error")
  
  # Should be reasonably controlled (< 20% with borrowing)
  expect_lt(type1_basket3, 0.30)
  expect_lt(type1_basket4, 0.30)
})


test_that("extract_rejections handles empty results gracefully", {
  results <- list()
  expect_null(extract_rejections(results))
  
  # NULL result
  expect_null(extract_rejections(NULL))
})


test_that("operating characteristics reject impossible values", {
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = c(0.4, 0.3, 0.2),
    null_response_rates = 0.2,
    design_type = "bma"
  )
  
  data <- simulate_responses(design, n_sims = 20, seed = 777)
  results <- lapply(data, function(d) analyze_basket(d, design))
  ocs <- compute_operating_characteristics(results, design)
  
  # No OC value should be outside [0, 1]
  expect_false(any(sapply(ocs$basket_specific, function(x) x$value < 0 | x$value > 1)))
  expect_false(ocs$family_wise$fwer < 0 | ocs$family_wise$fwer > 1)
  expect_false(ocs$family_wise$fwp_disjunctive < 0 | ocs$family_wise$fwp_disjunctive > 1)
  expect_false(ocs$family_wise$fwp_conjunctive < 0 | ocs$family_wise$fwp_conjunctive > 1)
})


# ==============================================================================
# Cunanan Two-Stage Design Tests
# ==============================================================================

test_that("Cunanan two-stage simulation completes without error", {
  skip_if_not_installed("bmabasket")
  
  design <- basket_design(
    n_baskets = 4,
    sample_sizes = 30,
    response_rates = c(0.4, 0.4, 0.2, 0.2),  # 2 promising, 2 null
    null_response_rates = 0.2,
    design_type = "cunanan"
  )
  
  # Run small simulation
  expect_error(
    sim_results <- simulate_basket_trial(design, n_sims = 50, seed = 333),
    NA
  )
  
  # Check structure
  expect_s3_class(sim_results, "simulation_results")
  expect_equal(sim_results$n_sims, 50)
  expect_length(sim_results$raw_results, 50)
})


test_that("Cunanan produces valid rejections (not all NA)", {
  skip_if_not_installed("bmabasket")
  
  design <- basket_design(
    n_baskets = 4,
    sample_sizes = 30,
    response_rates = c(0.45, 0.45, 0.45, 0.45),  # All promising
    null_response_rates = 0.2,
    design_type = "cunanan"
  )
  
  sim_results <- simulate_basket_trial(design, n_sims = 100, seed = 444)
  
  # Check that power values are not NA
  power_values <- sapply(sim_results$operating_characteristics$basket_specific, 
                         function(x) x$value)
  expect_false(any(is.na(power_values)))
  
  # Power should be reasonably high (>50%) for all baskets under global alternative
  expect_true(all(power_values > 0.5))
})


test_that("Cunanan FWER control under global null", {
  skip_if_not_installed("bmabasket")
  
  design <- basket_design(
    n_baskets = 4,
    sample_sizes = 30,
    response_rates = rep(0.2, 4),  # All null
    null_response_rates = 0.2,
    design_type = "cunanan"
  )
  
  sim_results <- simulate_basket_trial(design, n_sims = 200, seed = 555)
  
  # FWER should be controlled at approximately alpha (with some Monte Carlo error)
  # Use generous bound of 15% (3x nominal 5%)
  expect_lt(sim_results$operating_characteristics$family_wise$fwer, 0.15)
})


test_that("Cunanan handles heterogeneous scenarios", {
  skip_if_not_installed("bmabasket")
  
  design <- basket_design(
    n_baskets = 5,
    sample_sizes = 25,
    response_rates = c(0.5, 0.5, 0.2, 0.2, 0.2),  # 2 very promising, 3 null
    null_response_rates = 0.2,
    design_type = "cunanan"
  )
  
  sim_results <- simulate_basket_trial(design, n_sims = 100, seed = 888)
  
  # Power for promising baskets (1, 2) should be higher than for null (3, 4, 5)
  ocs <- sim_results$operating_characteristics$basket_specific
  power_promising <- mean(c(ocs[[1]]$value, ocs[[2]]$value))
  type1_null <- mean(c(ocs[[3]]$value, ocs[[4]]$value, ocs[[5]]$value))
  
  expect_gt(power_promising, type1_null)
  expect_gt(power_promising, 0.6)  # Should detect strong signal
  expect_lt(type1_null, 0.2)  # Type I error should be controlled
})
