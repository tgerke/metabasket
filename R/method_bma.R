#' Bayesian Model Averaging Analysis Wrapper
#'
#' @description
#' Analyzes basket trial data using Bayesian Model Averaging (BMA) 
#' from the bmabasket package (Psioda et al. 2021).
#'
#' @param data A basket_data object containing trial data
#' @param design A basket_design object with design_type = "bma"
#' @param ... Additional arguments (for S3 consistency)
#'
#' @return A bma_result object containing:
#'   \item{posterior_probs}{Posterior probabilities that each basket exceeds null rate}
#'   \item{posterior_means}{Posterior mean response rates for each basket}
#'   \item{rejections}{Logical vector indicating which baskets reject null}
#'   \item{basket_names}{Names of baskets}
#'   \item{alpha}{Significance threshold used}
#'   \item{raw_result}{Raw output from bmabasket::bma()}
#'
#' @details
#' The BMA method (Psioda et al. 2021) averages over all possible partitions 
#' of baskets, where baskets within the same partition share the same response 
#' rate. This provides adaptive information borrowing that is data-driven.
#'
#' The method requires the bmabasket package to be installed. Prior parameters:
#' - mu0: Prior mean (default 0.5)
#' - phi0: Prior dispersion (default 1)
#' - pmp0: Model probability parameter (default 1, use 0 for uniform prior)
#'
#' @references
#' Psioda MA, Xu J, Jiang Q, Ke C, Yang Z, Ibrahim JG (2021). 
#' Bayesian adaptive basket trial design using model averaging. 
#' Biostatistics, 22(1):19-34. doi:10.1093/biostatistics/kxz014
#'
#' @examples
#' \dontrun{
#' # Requires bmabasket package
#' library(bmabasket)
#' 
#' design <- basket_design(
#'   n_baskets = 4,
#'   sample_sizes = 25,
#'   null_response_rates = 0.20,
#'   design_type = "bma"
#' )
#' 
#' data <- basket_data(
#'   basket_names = paste0("Basket", 1:4),
#'   n_patients = c(25, 25, 25, 25),
#'   n_responses = c(8, 10, 6, 12)
#' )
#' 
#' result <- analyze_basket(data, design)
#' print(result)
#' }
#'
#' @export
analyze_basket.bma <- function(data, design, ...) {
  
  if (!requireNamespace("bmabasket", quietly = TRUE)) {
    stop("Package 'bmabasket' is required for BMA analysis. ",
         "Install it with: install.packages('bmabasket')",
         call. = FALSE)
  }
  
  # Validate inputs
  if (!inherits(data, "basket_data")) {
    stop("data must be a basket_data object")
  }
  
  if (!inherits(design, "basket_design") || design$design_type != "bma") {
    stop("design must be a basket_design object with design_type = 'bma'")
  }
  
  # Extract data
  y <- data$n_responses
  n <- data$n_patients
  pi0 <- design$null_response_rates
  
  # Get prior parameters from design_params or use defaults
  params <- design$design_params
  mu0 <- if (!is.null(params$mu0)) params$mu0 else 0.5
  phi0 <- if (!is.null(params$phi0)) params$phi0 else 1
  pmp0 <- if (!is.null(params$pmp0)) params$pmp0 else 1
  P <- if (!is.null(params$max_distinct)) params$max_distinct else design$n_baskets
  
  # Run BMA analysis (suppress benign normalization warnings)
  bma_result <- suppressWarnings(bmabasket::bma(
    pi0 = pi0,
    y = y,
    n = n,
    P = P,
    mu0 = mu0,
    phi0 = phi0,
    pmp0 = pmp0
  ))
  
  # Extract posterior probabilities and means (convert to vectors)
  posterior_probs <- as.vector(bma_result$bmaProbs)
  posterior_means <- as.vector(bma_result$bmaMeans)
  
  # Decision threshold (default 0.95 or from design_params)
  threshold <- if (!is.null(params$post_prob_threshold)) {
    params$post_prob_threshold
  } else {
    0.95
  }
  
  # Rejection decisions
  rejections <- posterior_probs > threshold
  
  # Package results
  result <- structure(
    list(
      posterior_probs = posterior_probs,
      posterior_means = posterior_means,
      rejections = rejections,
      basket_names = data$basket_names,
      threshold = threshold,
      alpha = threshold,  # For compatibility with extract_rejections
      raw_result = bma_result,
      design_params = list(
        mu0 = mu0,
        phi0 = phi0,
        pmp0 = pmp0,
        P = P
      )
    ),
    class = "bma_result"
  )
  
  return(result)
}


#' Print method for BMA results
#'
#' @param x A bma_result object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.bma_result <- function(x, ...) {
  cat("Bayesian Model Averaging (BMA) Analysis Results\n")
  cat("================================================\n\n")
  
  cat("Prior parameters:\n")
  cat("  mu0 (prior mean):", x$design_params$mu0, "\n")
  cat("  phi0 (prior dispersion):", x$design_params$phi0, "\n")
  cat("  pmp0 (model prob parameter):", x$design_params$pmp0, "\n")
  cat("  P (max distinct params):", x$design_params$P, "\n\n")
  
  cat("Decision threshold:", x$threshold, "\n\n")
  
  # Create results table
  results_df <- data.frame(
    Basket = x$basket_names,
    `Post Mean` = sprintf("%.3f", x$posterior_means),
    `Post Prob` = sprintf("%.3f", x$posterior_probs),
    Decision = ifelse(x$rejections, "REJECT H0", "FAIL TO REJECT"),
    check.names = FALSE
  )
  
  print(results_df, row.names = FALSE)
  
  cat("\n")
  cat("Baskets rejecting null:", sum(x$rejections), "of", length(x$rejections), "\n")
  
  invisible(x)
}


#' Extract rejection decisions from BMA results
#'
#' @param result A bma_result object from BMA analysis
#' @param ... Additional arguments (for S3 consistency)
#'
#' @return Logical vector indicating which baskets reject the null hypothesis
#'
#' @export
extract_rejections.bma_result <- function(result, ...) {
  if (!inherits(result, "bma_result")) {
    stop("result must be a bma_result object")
  }
  
  return(result$rejections)
}
