test_that("simulate_responses generates correct number of simulations", {
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = 0.30,
    null_response_rates = 0.15,
    design_type = "bma"
  )
  
  sims <- simulate_responses(design, n_sims = 10, seed = 123)
  
  expect_s3_class(sims, "basket_simulation")
  expect_length(sims, 10)
  expect_equal(attr(sims, "n_sims"), 10)
  
  # Check each simulation is a basket_data object
  for (i in 1:10) {
    expect_s3_class(sims[[i]], "basket_data")
    expect_equal(sims[[i]]$n_baskets, 3)
  }
})


test_that("simulate_responses produces reproducible results with seed", {
  
  design <- basket_design(
    n_baskets = 2,
    sample_sizes = 10,
    response_rates = 0.50,
    null_response_rates = 0.20,
    design_type = "bhm"
  )
  
  sims1 <- simulate_responses(design, n_sims = 5, seed = 456)
  sims2 <- simulate_responses(design, n_sims = 5, seed = 456)
  
  # Should get identical results with same seed
  for (i in 1:5) {
    expect_equal(sims1[[i]]$n_responses, sims2[[i]]$n_responses)
  }
})


test_that("simulate_responses respects response rate parameters", {
  
  # Test with extreme response rates
  design_high <- basket_design(
    n_baskets = 1,
    sample_sizes = 100,
    response_rates = 1.0,  # 100% response rate
    null_response_rates = 0.50,
    design_type = "bma"
  )
  
  design_low <- basket_design(
    n_baskets = 1,
    sample_sizes = 100,
    response_rates = 0.0,  # 0% response rate
    null_response_rates = 0.50,
    design_type = "bma"
  )
  
  sims_high <- simulate_responses(design_high, n_sims = 10, seed = 789)
  sims_low <- simulate_responses(design_low, n_sims = 10, seed = 890)
  
  # With p=1, should get n=100 responses in all sims
  for (i in 1:10) {
    expect_equal(sims_high[[i]]$n_responses, 100)
  }
  
  # With p=0, should get 0 responses in all sims
  for (i in 1:10) {
    expect_equal(sims_low[[i]]$n_responses, 0)
  }
})


test_that("simulate_responses handles varying basket sizes", {
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = c(10, 20, 30),
    response_rates = c(0.20, 0.30, 0.40),
    null_response_rates = 0.15,
    design_type = "mem"
  )
  
  sims <- simulate_responses(design, n_sims = 5, seed = 111)
  
  for (i in 1:5) {
    expect_equal(sims[[i]]$n_patients, c(10, 20, 30))
    # Responses should be between 0 and n_patients
    expect_true(all(sims[[i]]$n_responses >= 0))
    expect_true(all(sims[[i]]$n_responses <= sims[[i]]$n_patients))
  }
})
