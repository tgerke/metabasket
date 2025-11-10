test_that("basket_data creates valid data object", {
  
  # Create simple basket data
  data <- basket_data(
    basket_names = c("Basket1", "Basket2", "Basket3"),
    n_patients = c(20, 25, 30),
    n_responses = c(5, 8, 12)
  )
  
  expect_s3_class(data, "basket_data")
  expect_equal(data$n_baskets, 3)
  expect_length(data$basket_names, 3)
  expect_length(data$n_patients, 3)
  expect_length(data$n_responses, 3)
  expect_length(data$response_rates, 3)
  
  # Check response rate calculation
  expect_equal(data$response_rates[1], 5/20)
  expect_equal(data$response_rates[2], 8/25)
  expect_equal(data$response_rates[3], 12/30)
})


test_that("basket_data validates inputs", {
  
  # Test responses > patients
  expect_error(
    basket_data(
      basket_names = c("Basket1", "Basket2"),
      n_patients = c(20, 25),
      n_responses = c(25, 10)  # First basket has too many responses
    ),
    "Number of responses cannot exceed number of patients"
  )
  
  # Test length mismatch
  expect_error(
    basket_data(
      basket_names = c("Basket1", "Basket2"),
      n_patients = c(20, 25, 30),  # Too many
      n_responses = c(5, 8)
    ),
    "Length of n_patients must equal length of basket_names"
  )
})


test_that("basket_data matches imatinib trial data", {
  
  # Data from Chugh et al. 2009 (Table 1 in review paper)
  imatinib_data <- basket_data(
    basket_names = c("Angiosarcoma", "Ewing", "Fibrosarcoma", "Leiomyosarcoma",
                     "Liposarcoma", "MFH", "Osteosarcoma", "MPNST",
                     "Rhabdomyosarcoma", "Synovial"),
    n_patients = c(15, 13, 12, 28, 29, 29, 26, 5, 2, 20),
    n_responses = c(2, 0, 1, 6, 7, 3, 5, 1, 0, 3)
  )
  
  expect_equal(imatinib_data$n_baskets, 10)
  expect_equal(sum(imatinib_data$n_patients), 179)
  expect_equal(sum(imatinib_data$n_responses), 28)
  
  # Check overall response rate (should be 28/179 = 0.156)
  overall_rr <- sum(imatinib_data$n_responses) / sum(imatinib_data$n_patients)
  expect_equal(overall_rr, 28/179, tolerance = 0.001)
})


test_that("basket_data matches vemurafenib trial data", {
  
  # Data from Hyman et al. 2015 (Table 2 in review paper)
  vemurafenib_data <- basket_data(
    basket_names = c("ATC", "ECD/LCH", "CCA", "CRC-V", "CRC-VC", "NSCLC"),
    n_patients = c(7, 14, 8, 26, 10, 19),
    n_responses = c(2, 6, 1, 1, 0, 8)
  )
  
  expect_equal(vemurafenib_data$n_baskets, 6)
  expect_equal(sum(vemurafenib_data$n_patients), 84)
  expect_equal(sum(vemurafenib_data$n_responses), 18)
  
  # Check overall response rate (should be 18/84 = 0.214)
  overall_rr <- sum(vemurafenib_data$n_responses) / sum(vemurafenib_data$n_patients)
  expect_equal(overall_rr, 18/84, tolerance = 0.001)
  
  # Check specific basket response rates
  expect_equal(vemurafenib_data$response_rates[2], 6/14, tolerance = 0.001)  # ECD/LCH: 42.9%
  expect_equal(vemurafenib_data$response_rates[6], 8/19, tolerance = 0.001)  # NSCLC: 42.1%
})
