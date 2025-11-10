#' Create a basket trial design specification
#'
#' @description
#' Creates a structured specification for a basket trial design that can be used
#' across different analysis methods (BMA, hierarchical models, etc.)
#'
#' @param n_baskets Integer. Number of baskets (cancer types/subtypes) in the trial
#' @param basket_names Character vector. Names of the baskets
#' @param sample_sizes Integer vector. Sample size per basket (can be single value or vector)
#' @param response_rates Numeric vector. True response rates under alternative (for simulation)
#' @param null_response_rates Numeric vector. Null response rates to test against
#' @param design_type Character. Type of design: "bma" (Bayesian model averaging),
#'   "mem" (multi-source exchangeability), "bhm" (Bayesian hierarchical model),
#'   or "cunanan" (Cunanan et al. 2017)
#' @param design_params List. Additional design-specific parameters
#'
#' @return An object of class 'basket_design' containing the trial specification
#' @export
#'
#' @examples
#' # Create a simple 4-basket design
#' design <- basket_design(
#'   n_baskets = 4,
#'   basket_names = c("NSCLC", "SCLC", "Melanoma", "RCC"),
#'   sample_sizes = 25,
#'   response_rates = c(0.35, 0.35, 0.35, 0.35),
#'   null_response_rates = 0.20,
#'   design_type = "bma"
#' )
basket_design <- function(n_baskets,
                          basket_names = NULL,
                          sample_sizes,
                          response_rates = NULL,
                          null_response_rates,
                          design_type = c("bma", "mem", "bhm", "cunanan"),
                          design_params = list()) {
  
  design_type <- match.arg(design_type)
  
  # Set default basket names if not provided
  if (is.null(basket_names)) {
    basket_names <- paste0("Basket_", seq_len(n_baskets))
  }
  
  # Validate inputs
  if (length(basket_names) != n_baskets) {
    stop("Length of basket_names must equal n_baskets")
  }
  
  # Expand single values to vectors
  if (length(sample_sizes) == 1) {
    sample_sizes <- rep(sample_sizes, n_baskets)
  }
  
  # response_rates is optional (used for simulation but not required for analysis)
  if (!is.null(response_rates)) {
    if (length(response_rates) == 1) {
      response_rates <- rep(response_rates, n_baskets)
    }
  }
  
  if (length(null_response_rates) == 1) {
    null_response_rates <- rep(null_response_rates, n_baskets)
  }
  
  # Additional validation
  if (length(sample_sizes) != n_baskets) {
    stop("Length of sample_sizes must be 1 or equal to n_baskets")
  }
  
  if (!is.null(response_rates) && length(response_rates) != n_baskets) {
    stop("Length of response_rates must be 1 or equal to n_baskets")
  }
  
  if (length(null_response_rates) != n_baskets) {
    stop("Length of null_response_rates must be 1 or equal to n_baskets")
  }
  
  # Create the design object
  design <- structure(
    list(
      n_baskets = n_baskets,
      basket_names = basket_names,
      sample_sizes = sample_sizes,
      response_rates = response_rates,
      null_response_rates = null_response_rates,
      design_type = design_type,
      design_params = design_params
    ),
    class = "basket_design"
  )
  
  return(design)
}


#' Print method for basket_design
#'
#' @param x A basket_design object
#' @param ... Additional arguments (not used)
#' @export
print.basket_design <- function(x, ...) {
  cat("Basket Trial Design\n")
  cat("==================\n\n")
  cat("Design Type:", x$design_type, "\n")
  cat("Number of Baskets:", x$n_baskets, "\n\n")
  
  cat("Basket Details:\n")
  
  # Build data frame with available information
  basket_df <- data.frame(
    Basket = x$basket_names,
    N = x$sample_sizes,
    Null_Rate = x$null_response_rates
  )
  
  # Add response_rates only if provided
  if (!is.null(x$response_rates) && length(x$response_rates) > 0) {
    basket_df$Response_Rate = x$response_rates
  }
  
  print(basket_df, row.names = FALSE)
  
  if (length(x$design_params) > 0) {
    cat("\nDesign Parameters:\n")
    print(x$design_params)
  }
  
  invisible(x)
}


#' Create a basket trial data object
#'
#' @description
#' Creates a structured data object representing observed data from a basket trial
#'
#' @param basket_names Character vector. Names of the baskets
#' @param n_patients Integer vector. Number of patients per basket
#' @param n_responses Integer vector. Number of responses per basket
#'
#' @return An object of class 'basket_data' containing the trial data
#' @export
#'
#' @examples
#' # Imatinib trial data from Chugh et al. 2009
#' data <- basket_data(
#'   basket_names = c("Angiosarcoma", "Ewing", "Fibrosarcoma"),
#'   n_patients = c(15, 13, 12),
#'   n_responses = c(2, 0, 1)
#' )
basket_data <- function(basket_names, n_patients, n_responses) {
  
  n_baskets <- length(basket_names)
  
  # Validate inputs
  if (length(n_patients) != n_baskets) {
    stop("Length of n_patients must equal length of basket_names")
  }
  
  if (length(n_responses) != n_baskets) {
    stop("Length of n_responses must equal length of basket_names")
  }
  
  if (any(n_responses > n_patients)) {
    stop("Number of responses cannot exceed number of patients in any basket")
  }
  
  # Calculate observed response rates
  response_rates <- n_responses / n_patients
  
  # Create the data object
  data <- structure(
    list(
      n_baskets = n_baskets,
      basket_names = basket_names,
      n_patients = n_patients,
      n_responses = n_responses,
      response_rates = response_rates
    ),
    class = "basket_data"
  )
  
  return(data)
}


#' Print method for basket_data
#'
#' @param x A basket_data object
#' @param ... Additional arguments (not used)
#' @export
print.basket_data <- function(x, ...) {
  cat("Basket Trial Data\n")
  cat("=================\n\n")
  cat("Number of Baskets:", x$n_baskets, "\n\n")
  
  data_df <- data.frame(
    Basket = x$basket_names,
    N = x$n_patients,
    Responses = x$n_responses,
    Response_Rate = round(x$response_rates, 3)
  )
  print(data_df, row.names = FALSE)
  
  cat("\nTotal Patients:", sum(x$n_patients), "\n")
  cat("Total Responses:", sum(x$n_responses), "\n")
  cat("Overall Response Rate:", 
      round(sum(x$n_responses) / sum(x$n_patients), 3), "\n")
  
  invisible(x)
}
