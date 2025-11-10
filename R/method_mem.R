#' Multi-source Exchangeability Model (MEM) Analysis Wrapper
#'
#' @description
#' Analyzes basket trial data using the Multi-source Exchangeability Model (MEM) 
#' from the basket package (Hobbs & Landin 2018, Kaizer et al. 2018).
#'
#' @param data A basket_data object containing trial data
#' @param design A basket_design object with design_type = "mem"
#' @param ... Additional arguments (for S3 consistency)
#'
#' @return A mem_result object containing:
#'   \item{posterior_probs}{Posterior probabilities that each basket exceeds null rate}
#'   \item{posterior_means}{Posterior mean response rates for each basket}
#'   \item{posterior_medians}{Posterior median response rates for each basket}
#'   \item{hpd_intervals}{Highest posterior density intervals}
#'   \item{rejections}{Logical vector indicating which baskets reject null}
#'   \item{basket_names}{Names of baskets}
#'   \item{exchangeability_matrix}{Matrix of posterior exchangeability probabilities (PEP)}
#'   \item{alpha}{Significance threshold used}
#'   \item{raw_result}{Raw output from basket::mem_exact()}
#'
#' @details
#' The MEM approach learns the exchangeability structure directly from the data,
#' allowing for flexible information borrowing between similar baskets while
#' protecting against borrowing from dissimilar baskets.
#'
#' The method requires the basket package to be installed. Prior parameters:
#' - shape1: First shape parameter for beta prior (default 0.5)
#' - shape2: Second shape parameter for beta prior (default 0.5)
#' - prior: Prior inclusion probability matrix for basket pairs
#' - hpd_alpha: HPD credible interval level (default 0.05)
#'
#' @references
#' Hobbs BP, Landin R (2018). Bayesian basket trial design with exchangeability 
#' monitoring. Statistics in Medicine, 37(25):3557-3572.
#' 
#' Kaizer AM, Koopmeiners JS, Hobbs BP (2018). Bayesian hierarchical modeling 
#' based on multisource exchangeability. Biostatistics, 19(2):169-184.
#'
#' @examples
#' \dontrun{
#' # Requires basket package
#' library(basket)
#' 
#' design <- basket_design(
#'   n_baskets = 4,
#'   sample_sizes = 25,
#'   null_response_rates = 0.20,
#'   design_type = "mem"
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
analyze_basket.mem <- function(data, design, ...) {
  
  if (!requireNamespace("basket", quietly = TRUE)) {
    stop("Package 'basket' is required for MEM analysis. ",
         "Install it with: install.packages('basket')",
         call. = FALSE)
  }
  
  # Validate inputs
  if (!inherits(data, "basket_data")) {
    stop("data must be a basket_data object")
  }
  
  if (!inherits(design, "basket_design") || design$design_type != "mem") {
    stop("design must be a basket_design object with design_type = 'mem'")
  }
  
  # Extract data
  responses <- data$n_responses
  size <- data$n_patients
  name <- data$basket_names
  p0 <- design$null_response_rates
  
  # Get prior parameters from design_params or use defaults
  params <- design$design_params
  shape1 <- if (!is.null(params$shape1)) params$shape1 else 0.5
  shape2 <- if (!is.null(params$shape2)) params$shape2 else 0.5
  hpd_alpha <- if (!is.null(params$hpd_alpha)) params$hpd_alpha else 0.05
  
  # Prior inclusion probability matrix (default: 1 on diagonal, 0.5 elsewhere)
  if (!is.null(params$prior)) {
    prior <- params$prior
  } else {
    n <- length(responses)
    prior <- diag(n) / 2 + matrix(0.5, nrow = n, ncol = n)
  }
  
  # Run MEM analysis using basket::mem_exact()
  mem_result <- basket::mem_exact(
    responses = responses,
    size = size,
    name = name,
    p0 = p0,
    shape1 = shape1,
    shape2 = shape2,
    prior = prior,
    hpd_alpha = hpd_alpha,
    alternative = "greater",
    cluster_analysis = FALSE
  )
  
  # Extract results from basket object
  basket_obj <- mem_result$basket
  posterior_probs <- basket_obj$post_prob
  posterior_means <- basket_obj$mean_est
  posterior_medians <- basket_obj$median_est
  hpd_intervals <- basket_obj$hpd
  
  # Decision threshold (use HPD alpha for consistency)
  threshold <- 1 - hpd_alpha
  
  # Rejection decisions: reject if P(p > p0) > threshold
  rejections <- posterior_probs > threshold
  
  # Exchangeability matrix (PEP - posterior exchangeability probability)
  exchangeability_matrix <- basket_obj$pep
  
  # Package results
  result <- structure(
    list(
      posterior_probs = posterior_probs,
      posterior_means = posterior_means,
      posterior_medians = posterior_medians,
      hpd_intervals = hpd_intervals,
      rejections = rejections,
      basket_names = name,
      exchangeability_matrix = exchangeability_matrix,
      threshold = threshold,
      alpha = hpd_alpha,
      raw_result = mem_result,
      design_params = list(
        shape1 = shape1,
        shape2 = shape2,
        hpd_alpha = hpd_alpha,
        prior = prior
      )
    ),
    class = "mem_result"
  )
  
  return(result)
}


#' Print method for MEM results
#'
#' @param x A mem_result object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.mem_result <- function(x, ...) {
  cat("Multi-source Exchangeability Model (MEM) Analysis Results\n")
  cat("=========================================================\n\n")
  
  cat("Prior parameters:\n")
  cat("  shape1 (beta prior):", x$design_params$shape1, "\n")
  cat("  shape2 (beta prior):", x$design_params$shape2, "\n")
  cat("  hpd_alpha:", x$design_params$hpd_alpha, "\n\n")
  
  cat("Decision threshold (P > p0):", x$threshold, "\n\n")
  
  # Create results table
  results_df <- data.frame(
    Basket = x$basket_names,
    `Post Mean` = sprintf("%.3f", x$posterior_means),
    `Post Median` = sprintf("%.3f", x$posterior_medians),
    `Post Prob` = sprintf("%.3f", x$posterior_probs),
    `HPD Lower` = sprintf("%.3f", x$hpd_intervals[1, ]),
    `HPD Upper` = sprintf("%.3f", x$hpd_intervals[2, ]),
    Decision = ifelse(x$rejections, "REJECT H0", "FAIL TO REJECT"),
    check.names = FALSE
  )
  
  print(results_df, row.names = FALSE)
  
  cat("\n")
  cat("Baskets rejecting null:", sum(x$rejections), "of", length(x$rejections), "\n")
  
  # Show exchangeability information if available
  if (!is.null(x$exchangeability_matrix)) {
    cat("\nPosterior Exchangeability Probabilities (PEP):\n")
    cat("(Probability that baskets share the same response rate)\n")
    print(round(x$exchangeability_matrix, 3))
  }
  
  invisible(x)
}


#' Extract rejection decisions from MEM results
#'
#' @param result A mem_result object from MEM analysis
#' @param ... Additional arguments (for S3 consistency)
#'
#' @return Logical vector indicating which baskets reject the null hypothesis
#'
#' @export
extract_rejections.mem_result <- function(result, ...) {
  if (!inherits(result, "mem_result")) {
    stop("result must be a mem_result object")
  }
  
  return(result$rejections)
}
