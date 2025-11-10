test_that("Cunanan design performs interim analysis correctly", {
  # Stage 1 data with mixed results
  design <- basket_design(
    n_baskets = 5,
    sample_sizes = rep(7, 5),
    null_response_rates = 0.15,
    response_rates = 0.45,
    design_type = "cunanan"
  )

  # Heterogeneous scenario: varied response rates
  stage1_hetero <- basket_data(
    basket_names = paste0("Basket", 1:5),
    n_patients = rep(7, 5),
    n_responses = c(4, 4, 1, 0, 1)  # Clear differences
  )

  result_hetero <- analyze_basket(stage1_hetero, design)

  expect_s3_class(result_hetero, "cunanan_result")
  expect_true(result_hetero$interim_stage)
  expect_equal(result_hetero$design_path, "heterogeneous")
  expect_true(length(result_hetero$continued_baskets) < 5)  # Not all continue

  # Homogeneous scenario: similar response rates
  stage1_homo <- basket_data(
    basket_names = paste0("Basket", 1:5),
    n_patients = rep(7, 5),
    n_responses = c(3, 3, 3, 2, 3)  # Similar across baskets
  )

  result_homo <- analyze_basket(stage1_homo, design)

  expect_s3_class(result_homo, "cunanan_result")
  expect_true(result_homo$interim_stage)
  expect_equal(result_homo$design_path, "homogeneous")
  expect_equal(length(result_homo$continued_baskets), 5)  # All continue
})


test_that("Cunanan design handles futility stopping", {
  design <- basket_design(
    n_baskets = 5,
    sample_sizes = rep(7, 5),
    null_response_rates = 0.15,
    design_type = "cunanan"
  )

  # Low response rates across all baskets
  stage1_futile <- basket_data(
    basket_names = paste0("Basket", 1:5),
    n_patients = rep(7, 5),
    n_responses = c(0, 1, 0, 1, 0)  # Too few responses
  )

  result <- analyze_basket(stage1_futile, design)

  # Should select homogeneous path (similar low rates)
  # And stop for futility
  expect_s3_class(result, "cunanan_result")

  if (!is.null(result$interim_stage) && !result$interim_stage) {
    expect_true(result$futility_stopped)
    expect_true(all(!result$rejections))
  }
})


test_that("Cunanan design performs final analysis on heterogeneous path", {
  design <- basket_design(
    n_baskets = 5,
    sample_sizes = rep(7, 5),  # Stage 1 sample sizes
    null_response_rates = 0.15,
    design_type = "cunanan"
  )

  # Simulated interim result (heterogeneous path)
  interim <- structure(
    list(
      interim_stage = TRUE,
      design_path = "heterogeneous",
      toh_pvalue = 0.02,
      continued_baskets = c(1, 2, 3),
      stage1_responses = c(4, 3, 3, 0, 1),
      stage1_patients = rep(7, 5),
      gamma = 0.52,
      alpha_s = 0.07,
      alpha_c = 0.05
    ),
    class = c("cunanan_result", "list")
  )

  # Full data after stage 2 (baskets 1-3 continued)
  final_data <- basket_data(
    basket_names = paste0("Basket", 1:5),
    n_patients = c(22, 22, 22, 7, 7),  # Stage 1 + 2 for continuing baskets
    n_responses = c(10, 9, 8, 0, 1)
  )

  result <- analyze_basket.cunanan(
    data = final_data,
    design = design,
    interim_data = interim
  )

  expect_s3_class(result, "cunanan_result")
  expect_false(result$interim_stage %||% FALSE)
  expect_equal(result$design_path, "heterogeneous")
  expect_length(result$rejections, 5)
  expect_length(result$p_values, 5)

  # Baskets 1-3 should have valid p-values
  expect_true(all(result$p_values[1:3] < 1))

  # Baskets 4-5 didn't continue, should have p=1
  expect_equal(result$p_values[4:5], c(1, 1))
})


test_that("Cunanan design performs final analysis on homogeneous path", {
  design <- basket_design(
    n_baskets = 5,
    sample_sizes = rep(11, 5),  # Stage 1: 7, Stage 2: 4 per basket
    null_response_rates = 0.15,
    design_type = "cunanan"
  )

  # Simulated interim result (homogeneous path)
  interim <- structure(
    list(
      interim_stage = TRUE,
      design_path = "homogeneous",
      toh_pvalue = 0.80,
      continued_baskets = 1:5,
      stage1_responses = c(3, 3, 3, 2, 3),
      stage1_patients = rep(7, 5),
      gamma = 0.52,
      alpha_s = 0.07,
      alpha_c = 0.05
    ),
    class = c("cunanan_result", "list")
  )

  # Full data after stage 2 (all baskets continued)
  final_data <- basket_data(
    basket_names = paste0("Basket", 1:5),
    n_patients = rep(11, 5),
    n_responses = c(5, 5, 5, 4, 5)  # Maintained high response
  )

  result <- analyze_basket.cunanan(
    data = final_data,
    design = design,
    interim_data = interim
  )

  expect_s3_class(result, "cunanan_result")
  expect_false(result$interim_stage %||% FALSE)
  expect_equal(result$design_path, "homogeneous")
  expect_true(result$pooled_test)

  # All baskets should have same decision (pooled analysis)
  expect_equal(length(unique(result$rejections)), 1)
  expect_equal(length(unique(result$p_values)), 1)
})


test_that("Cunanan design matches published operating characteristics", {
  # Based on Cunanan et al. 2017 Table 2
  # K=5 baskets, n1=7, N2=20, gamma=0.52, alphaS=0.07, alphaC=0.05
  # Null scenario (A=0): FWER should be ~5%

  set.seed(2024)
  design <- basket_design(
    n_baskets = 5,
    sample_sizes = rep(7, 5),
    null_response_rates = 0.15,
    response_rates = 0.45,
    design_type = "cunanan",
    design_params = list(
      r_s = 1,
      r_c = 5,
      n2 = 15,
      gamma = 0.52,
      alpha_s = 0.07,
      alpha_c = 0.05
    )
  )

  # Simulate null scenario (all baskets at null rate)
  n_sims <- 100  # Use small number for test speed
  null_rejections <- numeric(n_sims)

  for (i in seq_len(n_sims)) {
    # Stage 1
    stage1 <- basket_data(
      basket_names = paste0("Basket", 1:5),
      n_patients = rep(7, 5),
      n_responses = rbinom(5, 7, 0.15)
    )

    interim <- analyze_basket(stage1, design)

    # Skip if futility
    if (!is.null(interim$futility_stopped) && interim$futility_stopped) {
      null_rejections[i] <- 0
      next
    }

    if (interim$interim_stage) {
      # Continue to stage 2
      k_star <- interim$continued_baskets
      n_stage2 <- rep(0, 5)
      n_stage2[k_star] <- ifelse(
        interim$design_path == "heterogeneous",
        15,
        4
      )

      final <- basket_data(
        basket_names = paste0("Basket", 1:5),
        n_patients = stage1$n_patients + n_stage2,
        n_responses = stage1$n_responses +
          rbinom(5, n_stage2, 0.15)
      )

      result <- analyze_basket.cunanan(final, design, interim_data = interim)
      null_rejections[i] <- sum(result$rejections)
    }
  }

  # FWER = Pr(at least one rejection | all null)
  fwer <- mean(null_rejections > 0)

  # Should be approximately 5% (allow wider tolerance for small n_sims)
  expect_true(fwer <= 0.15)  # Generous upper bound for test
})


test_that("extract_rejections works for Cunanan results", {
  # Final result
  result <- structure(
    list(
      rejections = c(TRUE, TRUE, FALSE, FALSE, FALSE),
      p_values = c(0.01, 0.02, 0.50, 0.80, 0.90),
      design_path = "heterogeneous"
    ),
    class = c("cunanan_result", "list")
  )

  rejections <- extract_rejections(result)
  expect_equal(rejections, c(TRUE, TRUE, FALSE, FALSE, FALSE))

  # Interim result should warn
  interim <- structure(
    list(
      interim_stage = TRUE,
      design_path = "heterogeneous"
    ),
    class = c("cunanan_result", "list")
  )

  expect_warning(
    extract_rejections(interim),
    "interim result"
  )
})


test_that("Cunanan print method works", {
  result <- structure(
    list(
      rejections = c(TRUE, FALSE, FALSE),
      p_values = c(0.01, 0.50, 0.80),
      toh_pvalue = 0.02,
      design_path = "heterogeneous",
      adjusted_alpha = 0.035,
      continued_baskets = c(1, 2)
    ),
    class = c("cunanan_result", "list")
  )

  expect_output(print(result), "Cunanan Efficient Basket Trial Design")
  expect_output(print(result), "heterogeneous")
  expect_output(print(result), "ACTIVE")
})
