test_that("basket_design creates valid design object", {
  
  # Test basic design creation
  design <- basket_design(
    n_baskets = 4,
    basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
    sample_sizes = 25,
    response_rates = 0.35,
    null_response_rates = 0.20,
    design_type = "bma"
  )
  
  expect_s3_class(design, "basket_design")
  expect_equal(design$n_baskets, 4)
  expect_length(design$basket_names, 4)
  expect_length(design$sample_sizes, 4)
  expect_length(design$response_rates, 4)
  expect_length(design$null_response_rates, 4)
  expect_equal(design$design_type, "bma")
})


test_that("basket_design validates inputs correctly", {
  
  # Test mismatched basket names
  expect_error(
    basket_design(
      n_baskets = 4,
      basket_names = c("NSCLC", "SCLC"),  # Only 2 names
      sample_sizes = 25,
      response_rates = 0.35,
      null_response_rates = 0.20,
      design_type = "bma"
    ),
    "Length of basket_names must equal n_baskets"
  )
  
  # Test invalid sample sizes
  expect_error(
    basket_design(
      n_baskets = 4,
      sample_sizes = c(25, 30),  # Wrong length
      response_rates = 0.35,
      null_response_rates = 0.20,
      design_type = "bma"
    ),
    "Length of sample_sizes must be 1 or equal to n_baskets"
  )
})


test_that("basket_design expands scalar inputs correctly", {
  
  design <- basket_design(
    n_baskets = 3,
    sample_sizes = 20,
    response_rates = 0.30,
    null_response_rates = 0.15,
    design_type = "bhm"
  )
  
  expect_length(design$sample_sizes, 3)
  expect_length(design$response_rates, 3)
  expect_length(design$null_response_rates, 3)
  expect_true(all(design$sample_sizes == 20))
  expect_true(all(design$response_rates == 0.30))
  expect_true(all(design$null_response_rates == 0.15))
})


test_that("basket_design accepts valid design types", {
  
  valid_types <- c("bma", "mem", "bhm", "cunanan")
  
  for (type in valid_types) {
    design <- basket_design(
      n_baskets = 2,
      sample_sizes = 20,
      response_rates = 0.30,
      null_response_rates = 0.15,
      design_type = type
    )
    expect_equal(design$design_type, type)
  }
  
  # Test invalid design type
  expect_error(
    basket_design(
      n_baskets = 2,
      sample_sizes = 20,
      response_rates = 0.30,
      null_response_rates = 0.15,
      design_type = "invalid"
    )
  )
})
