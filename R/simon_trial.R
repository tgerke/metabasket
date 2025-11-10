#' Create Simon two-stage design specification
#'
#' Creates a design object for parallel independent Simon two-stage designs.
#' This is not a basket design (no information borrowing), but is included
#' as a reference comparator to evaluate information borrowing methods.
#'
#' @param n_baskets Integer, number of independent cohorts/baskets
#' @param basket_names Optional character vector of cohort names
#' @param sample_sizes Integer vector of total sample sizes per cohort.
#'   If length 1, same size used for all cohorts.
#' @param null_response_rates Numeric vector of null response rates (p0).
#'   If length 1, same p0 used for all cohorts.
#' @param alternative_response_rates Numeric vector of alternative response rates (p1).
#'   If length 1, same p1 used for all cohorts. Must be > null_response_rates.
#' @param alpha Significance level per cohort (default 0.05)
#' @param beta Type II error rate per cohort (default 0.20)
#' @param design_type Character, "optimal" (default, minimizes EN) or
#'   "minimax" (minimizes maximum n)
#'
#' @return An object of class `simon_design` containing:
#'   * `n_baskets`: Number of independent cohorts
#'   * `basket_names`: Names of cohorts
#'   * `sample_sizes`: Total sample sizes
#'   * `null_response_rates`: Null response rates
#'   * `alpha`: Significance level
#'   * `beta`: Type II error rate
#'   * `design_type`: Design optimization criterion
#'
#' @details
#' Simon two-stage designs (Simon 1989) are the standard for single-arm phase II
#' trials. For basket trials, running parallel Simon designs provides a reference
#' that controls per-cohort error rates without borrowing information.
#'
#' Each cohort operates independently:
#' - Stage 1: Enroll n1 patients. Stop for futility if <= r1 responses.
#' - Stage 2: Enroll n2 = n - n1 additional patients.
#' - Decision: Reject H0 if total responses > r.
#'
#' Use Bonferroni correction for family-wise error control:
#' Set alpha = FWER / n_baskets.
#'
#' @references
#' Simon R (1989). Optimal two-stage designs for phase II clinical trials.
#' *Controlled Clinical Trials*, 10(1):1-10.
#'
#' @export
#' @examples
#' # Design for 4 independent cohorts
#' design <- simon_design(
#'   n_baskets = 4,
#'   basket_names = paste0("Cohort", 1:4),
#'   sample_sizes = 25,
#'   null_response_rates = 0.20,
#'   alternative_response_rates = 0.40,
#'   alpha = 0.05 / 4,  # Bonferroni for FWER control
#'   beta = 0.20
#' )
#'
#' print(design)
simon_design <- function(n_baskets,
                         basket_names = NULL,
                         sample_sizes,
                         null_response_rates,
                         alternative_response_rates,
                         alpha = 0.05,
                         beta = 0.20,
                         design_type = c("optimal", "minimax")) {
  
  design_type <- match.arg(design_type)
  
  # Validate inputs
  if (!is.numeric(n_baskets) || length(n_baskets) != 1 || n_baskets < 1) {
    stop("n_baskets must be a positive integer")
  }
  
  n_baskets <- as.integer(n_baskets)
  
  # Expand scalar inputs
  if (length(sample_sizes) == 1) {
    sample_sizes <- rep(sample_sizes, n_baskets)
  }
  
  if (length(null_response_rates) == 1) {
    null_response_rates <- rep(null_response_rates, n_baskets)
  }
  
  if (length(alternative_response_rates) == 1) {
    alternative_response_rates <- rep(alternative_response_rates, n_baskets)
  }
  
  # Validate lengths
  if (length(sample_sizes) != n_baskets) {
    stop("sample_sizes must have length 1 or n_baskets")
  }
  
  if (length(null_response_rates) != n_baskets) {
    stop("null_response_rates must have length 1 or n_baskets")
  }
  
  if (length(alternative_response_rates) != n_baskets) {
    stop("alternative_response_rates must have length 1 or n_baskets")
  }
  
  # Validate rates
  if (any(null_response_rates < 0 | null_response_rates > 1)) {
    stop("null_response_rates must be between 0 and 1")
  }
  
  if (any(alternative_response_rates < 0 | alternative_response_rates > 1)) {
    stop("alternative_response_rates must be between 0 and 1")
  }
  
  if (any(alternative_response_rates <= null_response_rates)) {
    stop("alternative_response_rates must be greater than null_response_rates")
  }
  
  # Generate basket names if not provided
  if (is.null(basket_names)) {
    basket_names <- paste0("Basket", seq_len(n_baskets))
  } else if (length(basket_names) != n_baskets) {
    stop("basket_names must have length n_baskets")
  }
  
  # Validate error rates
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha must be between 0 and 1")
  }
  
  if (beta <= 0 || beta >= 1) {
    stop("beta must be between 0 and 1")
  }
  
  structure(
    list(
      n_baskets = n_baskets,
      basket_names = basket_names,
      sample_sizes = as.integer(sample_sizes),
      null_response_rates = null_response_rates,
      alternative_response_rates = alternative_response_rates,
      alpha = alpha,
      beta = beta,
      design_type = design_type
    ),
    class = "simon_design"
  )
}


#' @export
print.simon_design <- function(x, ...) {
  cat("Simon Two-Stage Design (Parallel Independent Designs)\n")
  cat("=====================================================\n\n")
  
  cat("Design Type:", toupper(x$design_type), "\n")
  cat("  - optimal: minimizes expected sample size under H0\n")
  cat("  - minimax: minimizes maximum sample size\n\n")
  
  cat("Number of cohorts:", x$n_baskets, "\n")
  cat("Per-cohort error rates:\n")
  cat("  - Alpha (type I):", x$alpha, "\n")
  cat("  - Beta (type II):", x$beta, "\n")
  cat("  - Power:", 1 - x$beta, "\n\n")
  
  if (x$n_baskets <= 10) {
    cat("Cohort specifications:\n")
    for (i in seq_len(x$n_baskets)) {
      cat(sprintf("  %s: n=%d, p0=%.3f\n",
                  x$basket_names[i],
                  x$sample_sizes[i],
                  x$null_response_rates[i]))
    }
  } else {
    cat(sprintf("Sample sizes: %d to %d\n",
                min(x$sample_sizes), max(x$sample_sizes)))
    cat(sprintf("Null rates: %.3f to %.3f\n",
                min(x$null_response_rates), max(x$null_response_rates)))
  }
  
  cat("\nNote: No information borrowing between cohorts.\n")
  cat("For FWER control at level epsilon, use alpha = epsilon / n_baskets (Bonferroni).\n")
  
  invisible(x)
}
