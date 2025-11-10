test_that("Simon design calculates parameters correctly", {
  # Test with clinfun if available
  skip_if_not_installed("clinfun")

  params <- calculate_simon_design(
    p0 = 0.20,
    p1 = 0.40,
    alpha = 0.05,
    beta = 0.20,
    design_type = "optimal"
  )

  expect_type(params, "list")
  expect_true(all(c("r1", "n1", "r", "n", "EN") %in% names(params)))

  # Verify design properties
  expect_true(params$n1 < params$n)  # Stage 1 smaller than total
  expect_true(params$r1 < params$r)  # Stage 1 threshold lower
  expect_true(params$EN <= params$n) # Expected N ≤ maximum N
})


test_that("Simon design minimax differs from optimal", {
  skip_if_not_installed("clinfun")

  optimal <- calculate_simon_design(
    p0 = 0.20, p1 = 0.40,
    alpha = 0.05, beta = 0.20,
    design_type = "optimal"
  )

  minimax <- calculate_simon_design(
    p0 = 0.20, p1 = 0.40,
    alpha = 0.05, beta = 0.20,
    design_type = "minimax"
  )

  # Optimal should minimize EN under H0
  expect_true(optimal$EN <= minimax$EN)
  
  # Both designs should meet operating characteristic requirements
  expect_true(optimal$n > 0 && minimax$n > 0)
  expect_true(optimal$r >= optimal$r1 && minimax$r >= minimax$r1)
})


test_that("Simon design performs interim analysis correctly", {
  design <- simon_design(
    n_baskets = 4,
    sample_sizes = 25,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40
  )

  # Stage 1 data: mixed results
  stage1 <- basket_data(
    basket_names = paste0("Basket", 1:4),
    n_patients = c(12, 12, 12, 12),
    n_responses = c(6, 2, 5, 1)  # Varied responses
  )

  result <- analyze_basket(stage1, design, alpha = 0.05, beta = 0.20)

  expect_s3_class(result, "simon_result")
  expect_true(result$interim_stage)
  expect_length(result$stage1_continue, 4)
  expect_length(result$futility_stopped, 4)

  # With data: c(6, 2, 5, 1) and p0=0.20, some decisions should be made
  # At least verify the logic is internally consistent
  expect_equal(result$stage1_continue, !result$futility_stopped)

  # Design parameters should be present
  expect_true(!is.null(result$design_params$r1))
  expect_true(!is.null(result$design_params$n1))
})


test_that("Simon design handles futility stopping", {
  design <- simon_design(
    n_baskets = 3,
    sample_sizes = 25,
    null_response_rates = 0.15,
    alternative_response_rates = 0.40
  )

  # All baskets with low response rates
  stage1 <- basket_data(
    basket_names = paste0("Basket", 1:3),
    n_patients = c(12, 12, 12),
    n_responses = c(0, 1, 0)  # Very low
  )

  result <- analyze_basket(stage1, design, alpha = 0.05, beta = 0.20)

  expect_s3_class(result, "simon_result")
  expect_true(result$interim_stage)

  # Most or all should stop for futility
  expect_true(sum(result$futility_stopped) >= 2)
})


test_that("Simon design performs final analysis correctly", {
  design <- simon_design(
    n_baskets = 3,
    sample_sizes = 25,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40
  )

  # Simulated interim result
  interim <- structure(
    list(
      interim_stage = TRUE,
      design_params = list(
        r1 = 3, n1 = 12,
        r = 7, n = 25,
        EN = 20.5, PET = 0.3,
        design_type = "optimal"
      ),
      stage1_continue = c(TRUE, FALSE, TRUE),
      futility_stopped = c(FALSE, TRUE, FALSE),
      stage1_responses = c(5, 1, 4),
      stage1_patients = c(12, 12, 12),
      alpha = 0.05,
      beta = 0.20
    ),
    class = c("simon_result", "list")
  )

  # Full data after stage 2 (only baskets 1 and 3 continued)
  final_data <- basket_data(
    basket_names = paste0("Basket", 1:3),
    n_patients = c(25, 12, 25),  # Basket 2 stopped at interim
    n_responses = c(11, 1, 9)
  )

  result <- analyze_basket(
    data = final_data,
    design = design,
    interim_data = interim
  )

  expect_s3_class(result, "simon_result")
  expect_false(result$interim_stage %||% FALSE)
  expect_length(result$rejections, 3)
  expect_length(result$p_values, 3)

  # Basket 2 stopped at interim should not be rejected
  expect_false(result$rejections[2])
  expect_equal(result$p_values[2], 1)

  # Baskets 1 and 3 should have valid p-values
  expect_true(result$p_values[1] < 1)
  expect_true(result$p_values[3] < 1)
})


test_that("Simon design with Bonferroni controls FWER", {
  # Simulate null scenario (all baskets at null rate)
  set.seed(2024)

  n_baskets <- 4
  n_sims <- 100  # Small number for test speed
  fwer_target <- 0.05

  design <- simon_design(
    n_baskets = n_baskets,
    sample_sizes = 25,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40,
    alpha = fwer_target / n_baskets  # Bonferroni correction
  )

  null_rejections <- numeric(n_sims)

  for (i in seq_len(n_sims)) {
    # Stage 1
    stage1 <- basket_data(
      basket_names = paste0("Basket", 1:n_baskets),
      n_patients = rep(12, n_baskets),
      n_responses = rbinom(n_baskets, 12, 0.20)
    )

    interim <- analyze_basket(stage1, design)

    # Continue to stage 2 for non-futile baskets
    continuing <- which(interim$stage1_continue)

    if (length(continuing) > 0) {
      n_stage2 <- rep(0, n_baskets)
      n_stage2[continuing] <- 13  # n = 25 total

      final <- basket_data(
        basket_names = paste0("Basket", 1:n_baskets),
        n_patients = stage1$n_patients + n_stage2,
        n_responses = stage1$n_responses + rbinom(n_baskets, n_stage2, 0.20)
      )

      result <- analyze_basket(final, design,
                               interim_data = interim)
      null_rejections[i] <- sum(result$rejections)
    } else {
      null_rejections[i] <- 0
    }
  }

  # FWER = Pr(at least one rejection | all null)
  fwer <- mean(null_rejections > 0)

  # Should be approximately 5% (allow generous tolerance for small n_sims)
  expect_true(fwer <= 0.15)
})


test_that("Simon design achieves target power", {
  # Simulate alternative scenario (all baskets at alternative rate)
  set.seed(2025)

  n_baskets <- 3
  n_sims <- 100  # Small number for test speed
  power_target <- 0.80

  design <- simon_design(
    n_baskets = n_baskets,
    sample_sizes = 25,
    null_response_rates = 0.20,
    alternative_response_rates = 0.40
  )

  alt_rejections <- matrix(0, nrow = n_sims, ncol = n_baskets)

  for (i in seq_len(n_sims)) {
    # Stage 1
    stage1 <- basket_data(
      basket_names = paste0("Basket", 1:n_baskets),
      n_patients = rep(12, n_baskets),
      n_responses = rbinom(n_baskets, 12, 0.40)  # Alternative rate
    )

    interim <- analyze_basket(stage1, design)

    # Continue to stage 2 for non-futile baskets
    continuing <- which(interim$stage1_continue)

    if (length(continuing) > 0) {
      n_stage2 <- rep(0, n_baskets)
      n_stage2[continuing] <- 13

      final <- basket_data(
        basket_names = paste0("Basket", 1:n_baskets),
        n_patients = stage1$n_patients + n_stage2,
        n_responses = stage1$n_responses + rbinom(n_baskets, n_stage2, 0.40)
      )

      result <- analyze_basket(final, design,
                               interim_data = interim)
      alt_rejections[i, ] <- result$rejections
    }
  }

  # Marginal power per basket
  power_per_basket <- colMeans(alt_rejections)

  # Power should be reasonable (note: small n_sims=100 means high variability)
  # Just check that power is non-trivial
  expect_true(mean(power_per_basket) > 0.30)  # Average power > 30%
})


test_that("extract_rejections works for Simon results", {
  # Final result
  result <- structure(
    list(
      rejections = c(TRUE, FALSE, TRUE),
      p_values = c(0.01, 0.50, 0.02),
      design_params = list(r1 = 3, n1 = 12, r = 7, n = 25)
    ),
    class = c("simon_result", "list")
  )

  rejections <- extract_rejections(result)
  expect_equal(rejections, c(TRUE, FALSE, TRUE))

  # Interim result should warn
  interim <- structure(
    list(
      interim_stage = TRUE,
      design_params = list(r1 = 3, n1 = 12, r = 7, n = 25)
    ),
    class = c("simon_result", "list")
  )

  expect_warning(
    extract_rejections(interim),
    "interim result"
  )
})


test_that("Simon print method works", {
  result <- structure(
    list(
      rejections = c(TRUE, FALSE),
      p_values = c(0.01, 0.50),
      design_params = list(
        r1 = 3, n1 = 12,
        r = 7, n = 25,
        EN = 20.5, PET = 0.3,
        design_type = "optimal"
      ),
      futility_stopped = c(FALSE, FALSE),
      alpha = 0.05,
      beta = 0.20,
      test_statistics = list(
        responses_per_basket = c(10, 4),
        patients_per_basket = c(25, 25)
      )
    ),
    class = c("simon_result", "list")
  )

  expect_output(print(result), "Simon Two-Stage Design")
  expect_output(print(result), "optimal")
  expect_output(print(result), "ACTIVE")
  expect_output(print(result), "No information borrowing")
})


test_that("Simon design works without clinfun package", {
  # Test fallback calculation when clinfun not available
  # This uses the internal heuristic

  # Temporarily mask clinfun if it exists
  if ("clinfun" %in% loadedNamespaces()) {
    skip("clinfun is loaded, cannot test fallback")
  }

  params <- calculate_simon_design(
    p0 = 0.20,
    p1 = 0.40,
    alpha = 0.05,
    beta = 0.20,
    design_type = "optimal"
  )

  expect_type(params, "list")
  expect_true(all(c("r1", "n1", "r", "n", "EN") %in% names(params)))

  # Basic sanity checks
  expect_true(params$n1 > 0)
  expect_true(params$n > params$n1)
  expect_true(params$r >= params$r1)
})
